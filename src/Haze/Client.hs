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
import Control.Concurrent.Async (Async, async, cancel, link, race)
import Control.Exception.Safe (
    MonadThrow, MonadCatch, MonadMask,
    Exception, bracket, throw)
import Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Network.HTTP.Client
import qualified Network.Socket as Sock
import Network.Socket.ByteString (sendAllTo, recv)


import Data.TieredList (TieredList, popTiered)
import Haze.Bencoding (DecodeError(..), decode, decodeBen)
import Haze.Tracker (
    Tracker(..), MetaInfo(..), Announce(..), AnnounceInfo(..), 
    squashedTrackers, updateTransactionID,
    metaFromBytes, newTrackerRequest, trackerQuery,
    announceFromHTTP, parseUDPConn, parseUDPAnnounce,
    newUDPRequest, encodeUDPRequest)


{- | Generates a peer id from scratch.

Note that this should be generated before the first interaction with
a tracker, and not at every interaction with the tracker.

Uses the Azureus style id, with HZ as the prefix, and then appends
a UTC timestamp, before then taking only the first 20 bytes.
-}
generatePeerID :: MonadIO m => m ByteString
generatePeerID = liftIO $ do
    secs <- utctDayTime <$> getCurrentTime
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


-- | Represents errors that can happen with a tracker
data TrackerError
    -- | A malformatted packet was sent to the client
    = TrackerParseErr Text
    -- | The tracker sent an incorrect transaction ID
    | TrackerTransactionErr
    deriving (Show)

-- | This will get thrown because of a programmer error
data BadAnnounceException = BadAnnounceException Text
    deriving (Show)
instance Exception BadAnnounceException


type ConnMessage = Either TrackerError AnnounceInfo

-- | Throw an exception if the announce is bad
annConnMsg :: MonadThrow m => Either TrackerError Announce -> m ConnMessage
annConnMsg (Left e) =
    return (Left e)
annConnMsg (Right (FailedAnnounce t))  = 
    throw (BadAnnounceException t)
annConnMsg (Right (GoodAnnounce info)) =
    return (Right info)


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
    { clientTorrent :: MetaInfo
    -- | Used to communicate with the tracker connections
    , clientMsg :: MVar ConnMessage
    -- | This changes as the client rejects torrents
    , clientTrackers :: IORef (TieredList Tracker)
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
    -- | The tracker returned an error of some kind
    = BadTracker TrackerError
    -- | The attempt timed out
    | ScoutTimedOut
    -- | The scout was successful
    | ScoutSuccessful AnnounceInfo
    -- | We tried to connect to an unkown service
    | ScoutUnkownTracker Text


launchTorrent :: ClientM ()
launchTorrent = do
    peerID <- generatePeerID
    ClientInfo{..} <- ask
    let connInfo = ConnInfo peerID clientTorrent clientMsg
    loop connInfo
  where
    loop connInfo = do
        next <- popTracker
        ($ next) . maybe noTrackers $ \tracker -> do
            r <- tryTracker connInfo tracker
            case r of
                BadTracker err -> do
                    putTextLn ("Disconnecting from bad tracker:")
                    print err
                    loop connInfo
                ScoutTimedOut -> do
                    putTextLn "No response after 1s"
                    loop connInfo
                ScoutSuccessful info -> do
                    print info
                ScoutUnkownTracker t -> do
                    putTextLn "Skipping unkown tracker"
    tryTracker :: ConnInfo -> Tracker -> ClientM ScoutResult
    tryTracker connInfo tracker = do
        putTextLn ("Trying: " <> show tracker)
        let action = case tracker of
                HTTPTracker url -> 
                    Right . launchAsync . runConnWith connInfo $
                    connectHTTP url
                UDPTracker url prt -> 
                    Right . launchAsync . Sock.withSocketsDo . runConnWith connInfo $
                    connectUDP url prt
                UnknownTracker t ->
                    Left (ScoutUnkownTracker t)
        case action of
            Right thread -> do
                threadID <- thread
                mvar <- asks clientMsg
                res <- liftIO $ race (read mvar threadID) timeOut
                return (either id id res)
            Left res -> return res
    read :: MVar ConnMessage -> Async () -> IO ScoutResult
    read mvar thread = do
        ann <- takeMVar mvar
        case ann of
            Right info ->
                return (ScoutSuccessful info)
            Left err -> do
                cancel thread
                return (BadTracker err)
    timeOut :: IO ScoutResult
    timeOut = do
        threadDelay 1000000
        return ScoutTimedOut
    noTrackers :: MonadIO m => m ()
    noTrackers = putTextLn "No more trackers left to try :("
    launchAsync :: MonadIO m => IO () -> m (Async ())
    launchAsync m = liftIO $ do
        a <- async m
        link a
        return a


-- | The information a connection to a tracker needs
data ConnInfo = ConnInfo
    { connPeerID :: ByteString
    , connTorrent :: MetaInfo
    , connMsg :: MVar ConnMessage
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
putAnnounce :: Either TrackerError AnnounceInfo -> ConnM ()
putAnnounce (Left err) = do
    mvar <- asks connMsg
    putMVar mvar (Left err)
putAnnounce (Right info) = do
    mvar <- asks connMsg
    putMVar mvar (Right info)


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
            fullAnnounce = makeTrackerError (announceFromHTTP bytes)
        info <- annConnMsg fullAnnounce
        putAnnounce info
        let newTReq = updateReq trackerReq fullAnnounce
            time = (1000000 *) . fromRight 15 $ fmap annInterval info
        liftIO $ threadDelay time
        loop mgr req newTReq
    makeTrackerError (Left (DecodeError err)) = Left (TrackerParseErr err)
    makeTrackerError (Right x)                = Right x
    updateReq treq (Right (GoodAnnounce info)) =
        updateTransactionID (annTransactionID info) treq
    updateReq treq _    = treq


-- | Represents a UDP connection to some tracker
data UDPSocket = UDPSocket Sock.Socket Sock.SockAddr


-- | Connect to a UDP tracker with url and port
connectUDP :: Text -> Text -> ConnM ()
connectUDP url' prt' = do
    ConnInfo{..} <- ask
    bracket (makeUDPSocket url' prt') closeUDPSocket $ \udp -> do
        initiate connPeerID udp
        connBytes <- recvUDP udp 1024
        announce <- runExceptT $ do
            connInfo <- parseFail parseUDPConn connBytes
            let request = newUDPRequest connTorrent connPeerID connInfo
            sendUDP udp (encodeUDPRequest request)
            annBytes <- recvUDP udp 1024
            parseFail parseUDPAnnounce annBytes
        
        info <- annConnMsg announce
        putAnnounce info
    return () -- this seems to be necessary to force evaluation...
  where
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
    initiate :: MonadIO m => ByteString -> UDPSocket -> m ()
    initiate peerID udp = sendUDP udp $
      "\0\0\4\x17\x27\x10\x19\x80"
      <> "\0\0\0\0"
      -- This should be sufficiently unique
      <> BS.drop 16 peerID

parseFail :: Monad m => AP.Parser a -> ByteString -> ExceptT TrackerError m a
parseFail parser bs = ExceptT . return $
    case AP.parseOnly parser bs of
        Left s  -> Left (TrackerParseErr (fromString s))
        Right a -> Right a
