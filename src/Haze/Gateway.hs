{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{- |
Description: This module is responsible for letting peers join us.

This module contains the parts responsible for listening for new tcp
connections, as well as joining existing peers in the swarm.
-}
module Haze.Gateway
    ( GatewayInfo
    , makeGatewayInfo
    , GatewayM
    , runGatewayM
    , gatewayLoop
    )
where

import           Relude

import           Control.Concurrent.Async       ( async
                                                , withAsync
                                                )
import           Control.Concurrent.STM         ( retry )
import           Control.Concurrent.STM.TBQueue ( TBQueue
                                                , readTBQueue
                                                )
import           Control.Exception.Safe         ( MonadThrow
                                                , MonadCatch
                                                , MonadMask
                                                , bracket
                                                , finally
                                                )
import           Control.Monad.Random           ( MonadRandom )
import qualified Data.Attoparsec.ByteString    as AP
import qualified Data.ByteString               as BS
import qualified Data.HashMap.Strict           as HM
import qualified Network.Socket                as Net
import qualified Network.Simple.TCP            as TCP
import           System.Random.Shuffle          ( shuffleM )

import           Control.Logger                 ( HasLogger(..)
                                                , LoggerHandle
                                                , Importance(..)
                                                , (.=)
                                                , log
                                                )
import           Haze.Peer                      ( makePeerMInfo
                                                , runPeerM
                                                , startPeer
                                                )
import           Haze.PeerInfo                  ( PeerInfo(..)
                                                , HasPeerInfo(..)
                                                , addPeer
                                                )
import           Haze.Tracker                   ( AnnounceInfo(..)
                                                , MetaInfo(..)
                                                , PeerID(..)
                                                , Peer(..)
                                                , SHA1(..)
                                                , peerIDBytes
                                                )



{- | Represents the start of a handshake message

The entire handshake message also contains a PeerID, but usually
only the first part of the message is sent, and the PeerID follows
at a later time. Because of this, the message is split into the initial
portion, and then we read the PeerID seperately.
-}
newtype HandshakeStart = HandshakeStart
    { handshakeHash :: SHA1
    }

-- | Serialize a handshake start to a bytestring
handshakeBytes :: HandshakeStart -> ByteString
handshakeBytes (HandshakeStart hash) = mconcat
    [ BS.singleton 19
    , "BitTorrent protocol"
    , BS.pack (replicate 8 0)
    , getSHA1 hash
    ]

-- | A parser for the handshake start
parseHandshake :: AP.Parser HandshakeStart
parseHandshake = do
    void $ AP.word8 19
    void $ AP.string "BitTorrent protocol"
    void $ AP.take 8
    hash <- AP.take 20
    return (HandshakeStart (SHA1 hash))

-- | A parser for the peer id
parsePeerID :: AP.Parser PeerID
parsePeerID = PeerID <$> AP.take 20


-- | Maybe the result and the remaining unparsed bytes
type ParseRes a = Maybe (a, ByteString)

-- | Parse a full result from a socket, returning the leftover bytes
parseRecv
    :: MonadIO m => (ByteString -> AP.Result a) -> TCP.Socket -> m (ParseRes a)
parseRecv cb sock = do
    mBytes <- TCP.recv sock 1024
    case cb <$> mBytes of
        Nothing               -> return Nothing
        Just AP.Fail{}        -> return Nothing
        Just (AP.Partial cb') -> parseRecv cb' sock
        Just (AP.Done left a) -> return (Just (a, left))


-- | The maximum number of connections to initiate
maxActiveConnections :: Int
maxActiveConnections = 30

-- | The maximum number of connections to keep passively
maxPassiveConnections :: Int
maxPassiveConnections = 50


-- | Information the gateway needs
data GatewayInfo = GatewayInfo
    { gatewayPeerInfo :: !PeerInfo -- | General peer information
    -- | A Queue where we receive announce information
    , gatewayAnnounces :: !(TBQueue AnnounceInfo)
    -- | The torrent we're downloading
    , gatewayMeta :: !MetaInfo
    -- | The handle for our logger
    , gatewayLogger :: !LoggerHandle
    -- | The number of connections we currently have
    , gatewayConnections :: !(TVar Int)
    }

-- | Construct gateway information
makeGatewayInfo
    :: MonadIO m
    => PeerInfo
    -> TBQueue AnnounceInfo
    -> MetaInfo
    -> LoggerHandle
    -> m GatewayInfo
makeGatewayInfo info q meta lh = GatewayInfo info q meta lh <$> newTVarIO 0

-- | A computation with access to gateway information
newtype GatewayM a = GatewayM (ReaderT GatewayInfo IO a)
    deriving (Functor, Applicative, Monad,
              MonadReader GatewayInfo, MonadIO, MonadRandom,
              MonadThrow, MonadCatch, MonadMask)

instance HasPeerInfo GatewayM where
    getPeerInfo = asks gatewayPeerInfo

instance HasLogger GatewayM where
    getLogger = asks gatewayLogger

-- | Log something with the source set as the gateway
logGateway :: Importance -> [(Text, Text)] -> GatewayM ()
logGateway i pairs = log i ("source" .= ("gateway" :: String) : pairs)

-- | run a gateway computation given the right information
runGatewayM :: GatewayM a -> GatewayInfo -> IO a
runGatewayM (GatewayM m) = runReaderT m

{- | This is intended to be run no matter what at the end of a peer's cycle

This will close the socket connection provided, and decrement the amount
of active connections.
-}
cleanupConnection :: TCP.Socket -> GatewayM ()
cleanupConnection sock = do
    TCP.closeSock sock
    connections <- asks gatewayConnections
    atomically $ modifyTVar' connections (\x -> x - 1)

{- | Initiate a handshake given an intermediate computation

When we're connecting, we send our header before this function,
and our peer id inside. When accepting, we send our header inside.
-}
doHandshake :: Peer -> Bool -> TCP.Socket -> GatewayM ()
doHandshake peer client sock = void . runMaybeT $ do
    when client sendHeader
    (shake, left) <- MaybeT $ parseRecv (AP.parse parseHandshake) sock
    let theirHash = handshakeHash shake
    ourHash <- asks (metaInfoHash . gatewayMeta)
    when (theirHash /= ourHash) (fail "")
    if client then sendPeerID else sendHeader
    theirID <- case AP.parse parsePeerID left of
        AP.Fail{}     -> fail ""
        AP.Partial cb -> fmap fst . MaybeT $ parseRecv cb sock
        AP.Done _ r   -> return r
    unless (maybe True (== theirID) (peerID peer)) (fail "")
    unless client sendPeerID
    let peerWithId = peer { peerID = Just theirID }
    handle    <- MaybeT . fmap Just $ addPeer peerWithId
    logger    <- lift $ asks gatewayLogger
    peerMInfo <- makePeerMInfo sock peer logger handle
    lift $ logGateway
        Debug
        [ "msg" .= ("starting new peer" :: String)
        , "peer" .= peerWithId
        , "self-initiated" .= client
        ]
    liftIO $ runPeerM startPeer peerMInfo
  where
    sendHeader :: MaybeT GatewayM ()
    sendHeader = lift $ do
        hash <- asks (metaInfoHash . gatewayMeta)
        TCP.send sock (handshakeBytes (HandshakeStart hash))
    sendPeerID :: MaybeT GatewayM ()
    sendPeerID = lift $ do
        ourID <- asks (infoPeerID . gatewayPeerInfo)
        TCP.send sock (peerIDBytes ourID)


-- | Start the loop that listens for new passive connections
listenLoop :: GatewayM ()
listenLoop = TCP.listen TCP.HostAny "6881" $ \(mySock, _) -> do
    logGateway Info ["gateway-listening" .= (6881 :: Int)]
    allInfo <- ask
    liftIO $ go allInfo mySock
  where
    go :: GatewayInfo -> TCP.Socket -> IO ()
    go allInfo mySock = do
        let connections = gatewayConnections allInfo
        atomically $ do
            current <- readTVar connections
            if current >= maxPassiveConnections
                then retry
                else writeTVar connections (current + 1)
        TCP.accept mySock $ \h -> do
            let forked = runGatewayM (accept h) allInfo
            void . withAsync forked $ const (go allInfo mySock)
    accept handle@(sock, _) =
        handshakeWith handle `finally` cleanupConnection sock
    handshakeWith (sock, sockAddr) = do
        peer <- getPeer sock sockAddr
        doHandshake peer False sock
    getPeer :: MonadIO m => TCP.Socket -> TCP.SockAddr -> m Peer
    getPeer sock sockAddr = do
        let peerID = Nothing
        peerPort  <- liftIO $ Net.socketPort sock
        (host, _) <- liftIO $ getNameInfo sockAddr
        -- the host should always resolve
        let peerHost = fromMaybe (error "Unresolved host") host
        return Peer { .. }
    getNameInfo = Net.getNameInfo [Net.NI_NUMERICHOST] True False


-- | Start the gateway loop
gatewayLoop :: GatewayM ()
gatewayLoop = do
    allInfo <- ask
    let announce = runGatewayM announceLoop allInfo
        listen   = runGatewayM listenLoop allInfo
    liftIO (withAsync announce (const listen))


-- | Start the loop connecting to new peers after announces
announceLoop :: GatewayM ()
announceLoop = do
    q       <- asks gatewayAnnounces
    annInfo <- atomically $ readTBQueue q
    handleAnnounce annInfo
  where
    handleAnnounce :: AnnounceInfo -> GatewayM ()
    handleAnnounce AnnounceInfo {..} = do
        peerMap <- readTVarIO =<< asks (infoMap . gatewayPeerInfo)
        let newPeers = filter (not . (`HM.member` peerMap)) annPeers
        connections <- asks gatewayConnections
        allowed     <- atomically $ do
            current <- readTVar connections
            let left  = max 0 (maxActiveConnections - current)
                toAdd = min left (length newPeers)
            modifyTVar' connections (+ toAdd)
            return toAdd
        chosen  <- take allowed <$> shuffleM newPeers
        context <- ask
        logGateway DebugNoisy ["chosen" .= chosen]
        forM_ chosen $ \peer ->
            liftIO . void . async $ runGatewayM (connect peer) context
    connect :: Peer -> GatewayM ()
    connect peer = bracket (connectToPeer peer) cleanupConnection
        $ doHandshake peer True


{- | Initiate a connection to a peer

This just connects to the right TCP socket. This should be combined
with some form of bracketing to ensure proper cleanup.
-}
connectToPeer :: MonadIO m => Peer -> m TCP.Socket
connectToPeer Peer {..} = fst <$> TCP.connectSock peerHost (show peerPort)
