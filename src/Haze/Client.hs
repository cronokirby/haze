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
                                                , waitAnyCatchCancel
                                                )
import           Control.Concurrent.STM.TBQueue ( newTBQueueIO
                                                , readTBQueue
                                                )

import           Control.Logger                 ( defaultLoggerConfig
                                                , startLogger
                                                )
import           Haze.Announcer                 ( makeAnnouncerInfo
                                                , runAnnouncerM
                                                , launchAnnouncer
                                                )
import           Haze.Bencoding                 ( DecodeError(..) )
import           Haze.Tracker                   ( MetaInfo
                                                , metaFromBytes
                                                , firstTrackStatus
                                                )


launchClient :: FilePath -> IO ()
launchClient file = do
    bytes <- readFileBS file
    case metaFromBytes bytes of
        Left (DecodeError err) -> do
            putStrLn "Failed to decode file:"
            putTextLn err
        Right meta -> do
            pids     <- startAll meta
            (_, err) <- waitAnyCatchCancel pids
            putStrLn "Unexpected component crash: "
            print err

-- | Start all the sub components
startAll :: MetaInfo -> IO [Async ()]
startAll meta = do
    (logPID, logH) <- startLogger defaultLoggerConfig
    q              <- newTBQueueIO 16
    v              <- newTVarIO (firstTrackStatus meta)
    info           <- makeAnnouncerInfo meta v q logH
    announcerPID   <- async $ runAnnouncerM launchAnnouncer info
    printerPID     <- async . forever $ do
        ann <- atomically $ readTBQueue q
        print ann
    return [logPID, announcerPID, printerPID]

