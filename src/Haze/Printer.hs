{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE NumericUnderscores         #-}
{- |
Description: contains functions related to the Printer component

The printer component is responsible for presenting the user
of the program with a nice interface summarising the current
status of the torrent.
-}
module Haze.Printer
    ( PrinterInfo
    , makePrinterInfo
    , PrinterM
    , runPrinterM
    , printerLoop
    )
where

import           Relude

import           Control.Concurrent             ( threadDelay )
import qualified Data.HashMap.Strict           as HM
import qualified System.Console.ANSI           as ANSI
import           Text.Printf                    ( printf )

import           Haze.PeerInfo                  ( PeerInfo(..) )
import           Haze.Tracker                   ( MetaInfo
                                                , totalFileSize
                                                , TrackStatus(..)
                                                )


-- | Represents data about the status of the torrent
data StatusInfo = StatusInfo
    { statusUploaded :: !Int64 -- ^ the number of bytes uploaded
    -- | The total number of bytes downloaded
    , statusDownloaded :: !Int64
    -- | The total number of bytes in the torrent
    , statusToDownload :: !Int64
    -- | The rate at which we're downloading in bytes per second
    , statusDLRate :: !Double
    -- | The rate at which we're uploading in bytes per second
    , statusULRate :: !Double
    -- | The number of peers we're connected to
    , statusPeerCount :: !Int
    }

initialStatusInfo :: MetaInfo -> StatusInfo
initialStatusInfo meta = StatusInfo 0 0 (totalFileSize meta) 0.0 0.0 0


-- | Print the information in a status info to the console
printStatusInfo :: MonadIO m => StatusInfo -> m ()
printStatusInfo StatusInfo {..} = liftIO $ do
    printf "Connected to %d peers\n\n" statusPeerCount
    putStrLn "Uploaded:"
    let uploaded = printf "%.2f MB" (makeMB statusUploaded) :: String
    printf "%-24s %.2f MB/s\n\n" uploaded (statusULRate / 1_000_000)
    putStrLn "Downloaded:"
    let dl = makeMB statusDownloaded
        total = makeMB statusToDownload
        downloaded = printf "%.2f / %.2f MB" dl total :: String
    printf "%-24s %.2f MB/s\n" downloaded (statusDLRate / 1_000_000)
    liftIO $ ANSI.cursorUp 7
  where
    makeMB :: Integral a => a -> Double
    makeMB a = fromIntegral a / 1_000_000


-- | Represents all the information the printer needs
data PrinterInfo = PrinterInfo
    { printerStatus :: !(IORef StatusInfo)
    -- | The information about peers more generally
    , peerInfo :: !PeerInfo
    }

-- | Make the information a printer needs
makePrinterInfo :: MonadIO m => MetaInfo -> PeerInfo -> m PrinterInfo
makePrinterInfo meta peerInfo = do
    printerStatus <- newIORef (initialStatusInfo meta)
    return PrinterInfo { .. }

-- | The type of a context with access to a printer
newtype PrinterM a = PrinterM (ReaderT PrinterInfo IO a)
    deriving (Functor, Applicative, Monad,
              MonadReader PrinterInfo, MonadIO)

-- | Run a printer computation
runPrinterM :: PrinterM a -> PrinterInfo -> IO a
runPrinterM (PrinterM m) = runReaderT m

-- | How many times per second we should print
printFrequency :: Int
printFrequency = 4

-- | Update the current status information based on current info
updateStatus :: PrinterM ()
updateStatus = do
    PrinterInfo {..} <- ask
    let PeerInfo {..} = peerInfo
    (StatusInfo upld dld statusToDownload _ _ _) <- readIORef printerStatus
    trackStatus <- readTVarIO infoStatus
    let statusUploaded   = trackUp trackStatus
        statusDownloaded = trackDown trackStatus
        statusDLRate     = makeRate (statusDownloaded - dld)
        statusULRate     = makeRate (statusUploaded - upld)
    statusPeerCount <- HM.size <$> readTVarIO infoMap
    writeIORef printerStatus StatusInfo { .. }
  where
    makeRate :: Int64 -> Double
    makeRate x = fromIntegral printFrequency * fromIntegral x

printStatus :: PrinterM ()
printStatus = asks printerStatus >>= (readIORef >=> printStatusInfo)

-- | Loop, updating and printing the status
printerLoop :: PrinterM ()
printerLoop = forever $ do
    updateStatus
    printStatus
    liftIO $ threadDelay (1_000_000 `div` printFrequency)
