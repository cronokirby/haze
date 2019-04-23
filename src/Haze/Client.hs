{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{- | 
Description: contains functionality for starting the main client

The client is ultimately responsible for all the components
underneath it. It starts them all, 
-}
module Haze.Client
    ( launchClient
    )
where

import           Relude

import           Control.Concurrent.Async       ( Async
                                                , async
                                                , cancel
                                                , waitAnyCatchCancel
                                                )
import           Control.Concurrent.STM.TBQueue ( TBQueue
                                                , newTBQueueIO
                                                )
import           Control.Exception.Safe         ( finally )
import           Path                           ( Path
                                                , Abs
                                                , Dir
                                                )
import qualified Path

import           Control.Logger                 ( LoggerHandle
                                                , LoggerConfig(..)
                                                , defaultLoggerConfig
                                                , startLogger
                                                )
import           Haze.Announcer                 ( makeAnnouncerInfo
                                                , runAnnouncerM
                                                , launchAnnouncer
                                                )
import           Haze.Bencoding                 ( DecodeError(..) )
import           Haze.Config                    ( Config(..) )
import           Haze.Gateway                   ( makeGatewayInfo
                                                , runGatewayM
                                                , gatewayLoop
                                                )
import           Haze.PeerInfo                  ( PeerInfo(..)
                                                , makeEmptyPeerInfo
                                                )
import           Haze.PieceWriter               ( makePieceWriterInfo
                                                , runPieceWriterM
                                                , pieceWriterLoop
                                                )
import           Haze.Printer                   ( makePrinterInfo
                                                , runPrinterM
                                                , printerLoop
                                                )
import           Haze.Selector                  ( makeSelectorInfo
                                                , runSelectorM
                                                , selectorLoop
                                                )
import           Haze.Tracker                   ( AnnounceInfo
                                                , MetaInfo(..)
                                                , metaFromBytes
                                                )

-- | Represents all the information a client needs for orchestration
data ClientInfo = ClientInfo
    { clientPeerInfo :: !PeerInfo
    -- | The torrent file for this client
    , clientMeta :: !MetaInfo
    -- | The root we're using to save files
    , clientRoot :: !(Path Abs Dir)
    -- | The queue along which announce information is transmitted
    , clientAnnouncerResults :: !(TBQueue AnnounceInfo)
    -- | The logging handle
    , clientLogger :: !LoggerHandle
    }

-- | Make client information given a torrent file
makeClientInfo
    :: MonadIO m => MetaInfo -> Path Abs Dir -> LoggerHandle -> m ClientInfo
makeClientInfo clientMeta clientRoot clientLogger = do
    clientPeerInfo         <- makeEmptyPeerInfo clientMeta
    clientAnnouncerResults <- liftIO $ newTBQueueIO 16
    return ClientInfo { .. }

-- | Represents a context with access to client information
newtype ClientM a = ClientM (ReaderT ClientInfo IO a)
    deriving (Functor, Applicative, Monad,
              MonadReader ClientInfo, MonadIO)

-- | Run a client's computation
runClientM :: ClientM a -> ClientInfo -> IO a
runClientM (ClientM m) = runReaderT m

-- | Launch a client given a file path from which to start
launchClient :: Config -> IO ()
launchClient Config {..} = do
    bytes <- readFileBS (Path.fromAbsFile configTorrentFile)
    case metaFromBytes bytes of
        Left (DecodeError err) -> do
            putStrLn "Failed to decode file:"
            putTextLn err
        Right meta -> do
            let filename = Path.fromRelFile $ Path.filename configTorrentFile
            putStrLn ("Downloading " ++ filename ++ "\n")
            let loggerConfig =
                    defaultLoggerConfig { loggerFile = configLogFile }
            (pid, logger) <- startLogger loggerConfig
            clientInfo    <- makeClientInfo meta configDownloadDir logger
            runClientM startClient clientInfo `finally` cancel pid

startClient :: ClientM ()
startClient = do
    pids     <- startAll
    (_, err) <- liftIO $ waitAnyCatchCancel pids
    putStrLn "Unexpected component crash: "
    print err


-- | Start all the sub components
startAll :: ClientM [Async ()]
startAll = sequence
    [ startAnnouncer
    , startPieceWriter
    , startSelector
    , startGateway
    , startPrinter
    ]
  where
    asyncio = liftIO . async
    startAnnouncer :: ClientM (Async ())
    startAnnouncer = do
        meta   <- asks clientMeta
        peerID <- asks (infoPeerID . clientPeerInfo)
        status <- asks (infoStatus . clientPeerInfo)
        q      <- asks clientAnnouncerResults
        logH   <- asks clientLogger
        info   <- makeAnnouncerInfo meta peerID status q logH
        asyncio $ runAnnouncerM launchAnnouncer info
    startPieceWriter :: ClientM (Async ())
    startPieceWriter = do
        peerInfo <- asks clientPeerInfo
        fileInfo <- asks (metaFile . clientMeta)
        sha      <- asks (metaPieces . clientMeta)
        root     <- asks clientRoot
        logH     <- asks clientLogger
        pwInfo   <- makePieceWriterInfo peerInfo logH fileInfo sha root
        asyncio $ runPieceWriterM pieceWriterLoop pwInfo
    startSelector :: ClientM (Async ())
    startSelector = do
        peerInfo <- asks clientPeerInfo
        logger   <- asks clientLogger
        selInfo  <- makeSelectorInfo peerInfo logger
        asyncio $ runSelectorM selectorLoop selInfo
    startGateway :: ClientM (Async ())
    startGateway = do
        peerInfo  <- asks clientPeerInfo
        announces <- asks clientAnnouncerResults
        meta      <- asks clientMeta
        logger    <- asks clientLogger
        gateInfo  <- makeGatewayInfo peerInfo announces meta logger
        asyncio $ runGatewayM gatewayLoop gateInfo
    startPrinter :: ClientM (Async ())
    startPrinter = do
        peerInfo <- asks clientPeerInfo
        meta     <- asks clientMeta
        info     <- makePrinterInfo meta peerInfo
        asyncio $ runPrinterM printerLoop info
