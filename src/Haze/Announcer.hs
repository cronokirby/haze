{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{- | The Announcer is responsible for getting information from Trackers.

The main way of interacting with this component is by listening
to the 'AnnounceInfo' messages it produces. Because we only
listen to the announces as they arrive, we can be agnostic to changes
in the underlying tracker, for example.
-}
module Haze.Announcer
    ( AnnouncerInfo
    , AnnouncerM
    , makeAnnouncerInfo
    , runAnnouncerM
    , launchAnnouncer
    )
where

import           Relude

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( async
                                                , link
                                                , race
                                                )
import           Control.Concurrent.STM.TBQueue ( TBQueue
                                                , writeTBQueue
                                                )
import           Control.Exception.Safe         ( MonadThrow
                                                , MonadCatch
                                                , MonadMask
                                                , bracket
                                                , throwString
                                                )
import qualified Data.Attoparsec.ByteString    as AP
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.Time.Clock                ( DiffTime
                                                , getCurrentTime
                                                , utctDayTime
                                                )
import           Network.HTTP.Client
import qualified Network.Socket                as Sock
import           Network.Socket.ByteString      ( sendAllTo
                                                , recv
                                                )

import           Data.TieredList                ( TieredList
                                                , popTiered
                                                )
import           Haze.Bencoding                 ( DecodeError(..) )
import           Haze.Tracker                   ( Announce(..)
                                                , AnnounceInfo(..)
                                                , MetaInfo(..)
                                                , Tracker(..)
                                                , UDPTrackerRequest
                                                , UDPConnection(..)
                                                , squashedTrackers
                                                , updateTransactionID
                                                , newTrackerRequest
                                                , trackerQuery
                                                , announceFromHTTP
                                                , parseUDPConn
                                                , parseUDPAnnounce
                                                , newUDPRequest
                                                , encodeUDPRequest
                                                , updateUDPTransID
                                                )


-- | Get the current seconds part of a day
getSeconds :: MonadIO m => m DiffTime
getSeconds = liftIO $ utctDayTime <$> getCurrentTime


{- | Generates a peer id from scratch.

Note that this should be generated before the first interaction with
a tracker, and not at every interaction with the tracker.

Uses the Azureus style id, with HZ as the prefix, and then appends
a UTC timestamp, before then taking only the first 20 bytes.
-}
generatePeerID :: MonadIO m => m ByteString
generatePeerID = liftIO $ do
    secs <- getSeconds
    let whole = "-HZ010-" <> show secs
        cut   = BS.take 20 whole
    return cut

-- | Represents an error that can interrupt interaction with a Tracker
data AnnounceError
    -- | We couldn't parse a tracker's response
    = AnnounceFailedParse !Text
    -- | The tracker sent us a warning
    | AnnounceWarning !Text
    -- | The tracker had a mismatched transaction id
    | AnnounceBadTransaction
    deriving (Show)

-- | Represents the results of an announce with a Tracker
data AnnounceResult
    -- | We had no issues in getting an Announce
    = RealAnnounce !AnnounceInfo
    {- | Some error happened during the Announce

    This also indicates the death of the scout connected to that Tracker.
    -}
    | BadAnnounce !AnnounceError
    deriving (Show)

-- | Represents all the information an Announcer needs.
data AnnouncerInfo = AnnouncerInfo
    { announcerTorrent :: !MetaInfo
    -- | This allows us to report back successful announces
    , announcerResults :: !(TBQueue AnnounceInfo)
    -- | An 'MVar' is sufficient to receive 'AnnounceInfo' from our scout
    , announcerMsg :: !(MVar AnnounceResult)
    {- | This holds a list of Trackers we want to try and connect to.

    Only the thread launching scouts has access to this, so an IORef
    is fine.
    -}
    , announcerTrackers :: !(IORef (TieredList Tracker))
    }

makeAnnouncerInfo
    :: MonadIO m => MetaInfo -> TBQueue AnnounceInfo -> m AnnouncerInfo
makeAnnouncerInfo torrent results =
    AnnouncerInfo torrent results <$> newEmptyMVar <*> trackers
    where trackers = newIORef (squashedTrackers torrent)

-- | Represents the context for an announcer
newtype AnnouncerM a = AnnouncerM (ReaderT AnnouncerInfo IO a)
    deriving ( Functor, Applicative, Monad
             , MonadReader AnnouncerInfo, MonadIO
             )

-- | Run the context for an announcer
runAnnouncerM :: AnnouncerM a -> AnnouncerInfo -> IO a
runAnnouncerM (AnnouncerM m) = runReaderT m

-- | Report a successful announce back
reportAnnounceInfo :: AnnounceInfo -> AnnouncerM ()
reportAnnounceInfo info = do
    results <- asks announcerResults
    atomically $ writeTBQueue results info

-- | Tries to fetch the next tracker, popping it off the list
popTracker :: AnnouncerM (Maybe Tracker)
popTracker = do
    ref   <- asks announcerTrackers
    tiers <- readIORef ref
    case popTiered tiers of
        Nothing           -> return Nothing
        Just (next, rest) -> do
            writeIORef ref rest
            return (Just next)

-- | The result of an initial scout
data ScoutResult
    -- | The attempt timed out
    = ScoutTimedOut
    -- | The scout returned an initial result
    | ScoutReturned !AnnounceResult
    -- | We tried to connect to an unknown service
    | ScoutUnknownTracker !Text

launchAnnouncer :: AnnouncerM ()
launchAnnouncer = do
    peerID             <- generatePeerID
    AnnouncerInfo {..} <- ask
    let connInfo = ConnInfo peerID announcerTorrent announcerMsg
    scoutTrackers connInfo
  where
    scoutTrackers connInfo = forever $ do
        next <- popTracker
        ($ next) . maybe noTrackers $ \tracker -> do
            r <- tryTracker connInfo tracker
            case r of
                ScoutTimedOut ->
                    putTextLn "No response after 1s"
                ScoutReturned res ->
                    whenM (handleAnnounceRes res) settle
                ScoutUnknownTracker t ->
                    putTextLn ("Skipping unknown tracker " <> t)
    handleAnnounceRes :: AnnounceResult -> AnnouncerM Bool
    handleAnnounceRes res = case res of
        BadAnnounce  err  -> print err $> False
        RealAnnounce info -> reportAnnounceInfo info $> True
    settle :: AnnouncerM ()
    settle = do
        mvar <- asks announcerMsg
        res  <- liftIO $ readMVar mvar
        whenM (handleAnnounceRes res) settle
    tryTracker :: ConnInfo -> Tracker -> AnnouncerM ScoutResult
    tryTracker connInfo tracker = do
        putTextLn ("Trying: " <> show tracker)
        case tracker of
            HTTPTracker url ->
                connectHTTP url & runConnWith connInfo & launchConn
            UDPTracker url prt ->
                connectUDP url prt
                    & runConnWith connInfo
                    & Sock.withSocketsDo
                    & launchConn
            UnknownTracker t -> return (ScoutUnknownTracker t)
    noTrackers :: MonadIO m => m ()
    noTrackers = putTextLn "No more trackers left to try :("
    launchConn :: IO () -> AnnouncerM ScoutResult
    launchConn action = do
        void . liftIO $ async action
        mvar <- asks announcerMsg
        res  <- liftIO $ race (read mvar) timeOut
        return (either id id res)
    read :: MVar AnnounceResult -> IO ScoutResult
    read mvar = ScoutReturned <$> takeMVar mvar
    timeOut :: IO ScoutResult
    timeOut = do
        threadDelay 1000000
        return ScoutTimedOut


-- | The information a connection to a tracker needs
data ConnInfo = ConnInfo
    { connPeerID :: !ByteString
    , connTorrent :: !MetaInfo
    , connMsg :: !(MVar AnnounceResult)
    }

-- | Represent a context with access to a connection
newtype ConnM a = ConnM (ReaderT ConnInfo IO a)
    deriving ( Functor, Applicative, Monad
             , MonadReader ConnInfo, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             )

-- | Start a connection with certain information
runConnWith :: ConnInfo -> ConnM a -> IO a
runConnWith info (ConnM m) = runReaderT m info

-- | Send the information in an announce upstream
goodAnnounce :: AnnounceInfo -> ConnM ()
goodAnnounce info = do
    msgs <- asks connMsg
    putMVar msgs (RealAnnounce info)

-- | Send the bad results back and crash this process
badAnnounce :: AnnounceError -> ConnM a
badAnnounce err = do
    msgs <- asks connMsg
    putMVar msgs (BadAnnounce err)
    liftIO $ throwString "Bad announce from Tracker"

getAnnInfo :: Announce -> ConnM AnnounceInfo
getAnnInfo (FailedAnnounce t   ) = badAnnounce (AnnounceWarning t)
getAnnInfo (GoodAnnounce   info) = return info


connectHTTP :: Text -> ConnM ()
connectHTTP url = do
    ConnInfo {..} <- ask
    mgr           <- liftIO $ newManager defaultManagerSettings
    request       <- liftIO $ parseRequest (toString url)
    loop mgr request (newTrackerRequest connTorrent connPeerID)
  where
    loop mgr req trackerReq = do
        let query     = trackerQuery trackerReq
            withQuery = setQueryString query req
        response <- liftIO $ httpLbs withQuery mgr
        let bytes = LBS.toStrict $ responseBody response
        fullAnnounce <- case announceFromHTTP bytes of
            Left  (DecodeError err) -> badAnnounce (AnnounceFailedParse err)
            Right announce          -> return announce
        info <- getAnnInfo fullAnnounce
        goodAnnounce info
        let newTReq = updateReq trackerReq info
            time    = 1000000 * annInterval info
        liftIO $ threadDelay time
        loop mgr req newTReq
    updateReq treq info = updateTransactionID (annTransactionID info) treq


-- | Represents a UDP connection to some tracker
data UDPSocket = UDPSocket !Sock.Socket !Sock.SockAddr

-- | Connect to a UDP tracker with url and port
connectUDP :: Text -> Text -> ConnM ()
connectUDP url' prt' =
    void . bracket (makeUDPSocket url' prt') closeUDPSocket $ \udp -> do
        conn <- connect udp
        now  <- getSeconds
        req  <- makeUDPRequest conn
        loop udp req now
  where
    loop udp request lastConn = do
        now             <- getSeconds
        (req, connTime) <- if (now - lastConn) > 1
            then do
                conn     <- connect udp
                connTime <- getSeconds
                req      <- makeUDPRequest conn
                return (req, connTime)
            else return (request, lastConn)
        info <- getAnnounce udp req
        let newReq = updateReq info request
            time   = 1000000 * annInterval info
        liftIO $ threadDelay time
        loop udp newReq connTime
    connect :: UDPSocket -> ConnM UDPConnection
    connect udp = do
        peerID <- asks connPeerID
        let magicBytes =
                "\0\0\4\x17\x27\x10\x19\x80" <> "\0\0\0\0" <> BS.drop 16 peerID
        sendUDP udp magicBytes
        connBytes <- recvUDP udp 1024
        parseFail parseUDPConn connBytes
    makeUDPRequest :: UDPConnection -> ConnM UDPTrackerRequest
    makeUDPRequest conn = do
        ConnInfo {..} <- ask
        return (newUDPRequest connTorrent connPeerID conn)
    getAnnounce :: UDPSocket -> UDPTrackerRequest -> ConnM AnnounceInfo
    getAnnounce udp request = do
        sendUDP udp (encodeUDPRequest request)
        annBytes <- recvUDP udp 1024
        announce <- parseFail parseUDPAnnounce annBytes
        info     <- getAnnInfo announce
        goodAnnounce info
        return info
    updateReq :: AnnounceInfo -> UDPTrackerRequest -> UDPTrackerRequest
    updateReq info req =
        maybe req (`updateUDPTransID` req) (annTransactionID info)
    makeUDPSocket :: MonadIO m => Text -> Text -> m UDPSocket
    makeUDPSocket url prt = liftIO $ do
        let urlS  = toString url
            portS = toString prt
            hints =
                Just Sock.defaultHints { Sock.addrSocketType = Sock.Datagram }
        target : _ <- Sock.getAddrInfo hints (Just urlS) (Just portS)
        let fam  = Sock.addrFamily target
            addr = Sock.addrAddress target
        sock <- Sock.socket fam Sock.Datagram Sock.defaultProtocol
        return (UDPSocket sock addr)
    closeUDPSocket :: MonadIO m => UDPSocket -> m ()
    closeUDPSocket (UDPSocket sock _) = liftIO $ Sock.close sock
    recvUDP :: MonadIO m => UDPSocket -> Int -> m ByteString
    recvUDP (UDPSocket sock _) amount = liftIO $ recv sock amount
    sendUDP :: MonadIO m => UDPSocket -> ByteString -> m ()
    sendUDP (UDPSocket sock addr) bytes = liftIO $ sendAllTo sock bytes addr

parseFail :: AP.Parser a -> ByteString -> ConnM a
parseFail parser bs = case AP.parseOnly parser bs of
    Left  s -> badAnnounce (AnnounceFailedParse (fromString s))
    Right a -> return a
