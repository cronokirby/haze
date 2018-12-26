{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{- | 
Description: contains functions for interacting with a tracker

This exports IO actions for ineracting with a tracker, including
connecting and maintaining communication lines.
-}
module Haze.Client
    ( launchClient
    )
where

import Relude

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, link, race)
import Control.Exception.Safe (
    MonadThrow, MonadCatch, MonadMask,
    Exception, bracket, throw)
import Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (DiffTime, getCurrentTime, utctDayTime)
import Network.HTTP.Client
import qualified Network.Socket as Sock
import Network.Socket.ByteString (sendAllTo, recv)


import Data.TieredList (TieredList, popTiered)
import Haze.Bencoding (DecodeError(..))
import Haze.Tracker (
    Tracker(..), MetaInfo(..), Announce(..), AnnounceInfo(..), 
    UDPTrackerRequest, UDPConnection,
    squashedTrackers, updateTransactionID,
    metaFromBytes, newTrackerRequest, trackerQuery,
    announceFromHTTP, parseUDPConn, parseUDPAnnounce,
    newUDPRequest, encodeUDPRequest, updateUDPTransID)



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
        cut = BS.take 20 whole
    return cut


launchClient :: FilePath -> IO ()
launchClient file = do
    bytes <- readFileBS file
    case metaFromBytes bytes of
        Left (DecodeError err) -> do
            putStrLn "Failed to decode file:"
            putTextLn err
        Right meta -> do
            clientInfo <- newClientInfo meta
            runClientM launchTorrent clientInfo


-- | Represents the exceptions that can get thrown from a connection
data BadAnnounceException 
    -- | We couldn't parse a tracker response
    = BadParse !Text 
    -- | The announce had a warning
    | BadAnnounce !Text
    -- | The transaction ID was mismatched
    | BadTransaction
    deriving (Show)
instance Exception BadAnnounceException

-- | Get the announce info by throwing an exception
getAnnInfo :: MonadThrow m => Announce -> m AnnounceInfo
getAnnInfo (FailedAnnounce t)  = throw (BadAnnounce t)
getAnnInfo (GoodAnnounce info) = return info

{- | Represents the state of the client

Note that this is the state of the
entire client, and not a connection to
a tracker.

An MVar is fine for communication here, since
the parent thread is always waiting for
a message from the tracker connection, and the
tracker connection's messages are always spaced far
enough away to give us time to launch other things
in reaction to a message.
-}
data ClientInfo = ClientInfo
    { clientTorrent :: !MetaInfo
    -- | Used to communicate with the tracker connections
    , clientMsg :: !(MVar AnnounceInfo)
    -- | This changes as the client rejects torrents
    , clientTrackers :: !(IORef (TieredList Tracker))
    }

{- | Make a ClientInfo from a torrent with an empty Queue

The default queue size here is 16, which should be more
than sufficient, since we only ever expect one message
to be sent, and we're always waiting    
-}
newClientInfo :: MetaInfo -> IO ClientInfo
newClientInfo torrent = 
    ClientInfo torrent <$> newEmptyMVar <*> trackers
  where
    trackers = 
        newIORef (squashedTrackers torrent)

-- | Represents a global torrent client
newtype ClientM a = ClientM (ReaderT ClientInfo IO a)
    deriving ( Functor, Applicative, Monad
             , MonadReader ClientInfo, MonadIO
             )


-- | Runs a global client
runClientM :: ClientM a -> ClientInfo -> IO a
runClientM (ClientM m) = runReaderT m

-- | Tries to fetch the next tracker, popping it off the list
popTracker :: ClientM (Maybe Tracker)
popTracker = do
    ref <- asks clientTrackers
    tiers <- readIORef ref
    case popTiered tiers of
        Nothing -> return Nothing
        Just (next, rest) -> do
            writeIORef ref rest
            return (Just next)


-- | The result of an initial scout
data ScoutResult
    -- | The attempt timed out
    = ScoutTimedOut
    -- | The scout was successful
    | ScoutSuccessful !AnnounceInfo
    -- | We tried to connect to an unkown service
    | ScoutUnkownTracker !Text


launchTorrent :: ClientM ()
launchTorrent = do
    peerID <- generatePeerID
    ClientInfo{..} <- ask
    let connInfo = ConnInfo peerID clientTorrent clientMsg
    scoutTrackers connInfo
  where
    scoutTrackers connInfo = do
        next <- popTracker
        ($ next) . maybe noTrackers $ \tracker -> do
            r <- tryTracker connInfo tracker
            case r of
                ScoutTimedOut -> do
                    putTextLn "No response after 1s"
                    scoutTrackers connInfo
                ScoutSuccessful info -> do
                    print info
                    settle
                ScoutUnkownTracker _ -> do
                    putTextLn "Skipping unkown tracker"
                    scoutTrackers connInfo
    settle :: ClientM ()
    settle = forever $ do
        mvar <- asks clientMsg
        info <- liftIO $ readMVar mvar
        print info
    tryTracker :: ConnInfo -> Tracker -> ClientM ScoutResult
    tryTracker connInfo tracker = do
        putTextLn ("Trying: " <> show tracker)
        case tracker of
            HTTPTracker url -> 
                launchConn  . runConnWith connInfo $
                connectHTTP url
            UDPTracker url prt -> 
                launchConn . Sock.withSocketsDo . runConnWith connInfo $
                connectUDP url prt
            UnknownTracker t ->
                return (ScoutUnkownTracker t)
    noTrackers :: MonadIO m => m ()
    noTrackers = putTextLn "No more trackers left to try :("
    launchConn :: IO () -> ClientM ScoutResult
    launchConn action = do
        liftIO $ async action >>= link
        mvar <- asks clientMsg
        res <- liftIO $ race (read mvar) timeOut
        return (either id id res)
    read :: MVar AnnounceInfo -> IO ScoutResult
    read mvar = ScoutSuccessful <$> takeMVar mvar
    timeOut :: IO ScoutResult
    timeOut = do
        threadDelay 1000000
        return ScoutTimedOut


-- | The information a connection to a tracker needs
data ConnInfo = ConnInfo
    { connPeerID :: ByteString
    , connTorrent :: MetaInfo
    , connMsg :: MVar AnnounceInfo
    }


newtype ConnM a = ConnM (ReaderT ConnInfo IO a)
    deriving ( Functor, Applicative, Monad
             , MonadReader ConnInfo, MonadIO
             , MonadThrow, MonadCatch, MonadMask
             )

-- | Start a connection with certain information
runConnWith :: ConnInfo -> ConnM a -> IO a
runConnWith info (ConnM m) = runReaderT m info

{- | Send the information in an announce, or fail

Since bad announces are usually our fault, we want
to throw an exception
-}
putAnnounce :: AnnounceInfo -> ConnM ()
putAnnounce info = asks connMsg >>= (`putMVar` info)


connectHTTP :: Text -> ConnM ()
connectHTTP url = do
    ConnInfo{..} <- ask
    mgr <- liftIO $ newManager defaultManagerSettings
    request <- liftIO $ parseRequest (toString url)
    loop mgr request (newTrackerRequest connTorrent connPeerID)
  where
    loop mgr req trackerReq = do
        let query     = trackerQuery trackerReq
            withQuery = setQueryString query req
        response <- liftIO $ httpLbs withQuery mgr
        let bytes = LBS.toStrict $ responseBody response
        fullAnnounce <- case announceFromHTTP bytes of
                Left (DecodeError err) -> throw (BadParse err)
                Right announce -> return announce
        info <- getAnnInfo fullAnnounce
        putAnnounce info
        let newTReq = updateReq trackerReq info
            time = 1000000 * annInterval info
        liftIO $ threadDelay time
        loop mgr req newTReq
    updateReq treq info =
        updateTransactionID (annTransactionID info) treq


-- | Represents a UDP connection to some tracker
data UDPSocket = UDPSocket !Sock.Socket !Sock.SockAddr

-- | Connect to a UDP tracker with url and port
connectUDP :: Text -> Text -> ConnM ()
connectUDP url' prt' =
    void . bracket (makeUDPSocket url' prt') closeUDPSocket $ \udp -> do
        conn <- connect udp
        now <- getSeconds
        req <- makeUDPRequest conn
        loop udp req now
  where
    loop udp request lastConn = do
        now <- getSeconds
        (req, connTime) <- if (now - lastConn) > 1
            then do
                conn <- connect udp
                connTime <- getSeconds
                req <- makeUDPRequest conn
                return (req, connTime)
            else
                return (request, lastConn)
        info <- getAnnounce udp req
        let newReq = updateReq info request
            time = 1000000 * annInterval info
        liftIO $ threadDelay time
        loop udp newReq connTime
    connect :: UDPSocket -> ConnM UDPConnection
    connect udp = do
        peerID <- asks connPeerID
        let magicBytes = "\0\0\4\x17\x27\x10\x19\x80"
                <> "\0\0\0\0" <> BS.drop 16 peerID
        sendUDP udp magicBytes
        connBytes <- recvUDP udp 1024
        parseFail parseUDPConn connBytes
    makeUDPRequest :: UDPConnection -> ConnM UDPTrackerRequest
    makeUDPRequest conn = do
        ConnInfo{..} <- ask
        return (newUDPRequest connTorrent connPeerID conn)
    getAnnounce :: UDPSocket -> UDPTrackerRequest -> ConnM AnnounceInfo
    getAnnounce udp request = do
        sendUDP udp (encodeUDPRequest request)
        annBytes <- recvUDP udp 1024
        announce <- parseFail parseUDPAnnounce annBytes
        info <- getAnnInfo announce
        putAnnounce info
        return info
    updateReq :: AnnounceInfo -> UDPTrackerRequest -> UDPTrackerRequest
    updateReq info req = maybe req
        (`updateUDPTransID` req)
        (annTransactionID info)
    makeUDPSocket :: MonadIO m => Text -> Text -> m UDPSocket
    makeUDPSocket url prt = liftIO $ do
        let urlS =  toString url
            portS = toString prt
            hints = Just Sock.defaultHints
                { Sock.addrSocketType = Sock.Datagram 
                }
        target:_ <- Sock.getAddrInfo hints (Just urlS) (Just portS)
        let fam  = Sock.addrFamily target
            addr = Sock.addrAddress target
        sock <- Sock.socket fam Sock.Datagram Sock.defaultProtocol
        return (UDPSocket sock addr)
    closeUDPSocket :: MonadIO m => UDPSocket -> m ()
    closeUDPSocket (UDPSocket sock _) = liftIO $ Sock.close sock
    recvUDP :: MonadIO m => UDPSocket -> Int -> m ByteString
    recvUDP (UDPSocket sock _) amount = liftIO $ recv sock amount
    sendUDP :: MonadIO m => UDPSocket -> ByteString -> m ()
    sendUDP (UDPSocket sock addr ) bytes = liftIO $
        sendAllTo sock bytes addr


parseFail :: MonadThrow m => AP.Parser a -> ByteString -> m a
parseFail parser bs =
    case AP.parseOnly parser bs of
        Left s  -> throw (BadParse (fromString s))
        Right a -> return a
