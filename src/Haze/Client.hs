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

import           Relude

import           Control.Concurrent.Async       ( async )
import           Control.Concurrent.STM.TBQueue ( newTBQueueIO
                                                , readTBQueue
                                                )

import           Haze.Announcer                 ( makeAnnouncerInfo
                                                , runAnnouncerM
                                                , launchAnnouncer
                                                )
import           Haze.Bencoding                 ( DecodeError(..) )
import           Haze.Tracker                   ( metaFromBytes )


launchClient :: FilePath -> IO ()
launchClient file = do
    bytes <- readFileBS file
    case metaFromBytes bytes of
        Left (DecodeError err) -> do
            putStrLn "Failed to decode file:"
            putTextLn err
        Right meta -> do
            q    <- newTBQueueIO 16
            info <- makeAnnouncerInfo meta q
            void . async $ runAnnouncerM launchAnnouncer info
            forever $ do
                ann <- atomically $ readTBQueue q
                print ann
