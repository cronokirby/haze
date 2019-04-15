{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{- |
Description: Contains functions for working with a structured Logger

This is outside the Haze hierarchy as it doesn't contain anything
specific to Bittorrent.
-}
module Control.Logger
    ( Importance(..)
    , (.=)
    , LoggerConfig(..)
    , defaultLoggerConfig
    , startLogger
    , LoggerHandle
    , HasLogger(..)
    , log
    )
where

import           Relude

import           Control.Concurrent.Async       ( Async
                                                , async
                                                )
import           Control.Concurrent.STM.TBQueue ( TBQueue
                                                , newTBQueueIO
                                                , readTBQueue
                                                , writeTBQueue
                                                )
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import qualified Data.Text                     as Text


{- | Represents the importance of an event

Can be used to not log events that are of a certain importance.
-}
data Importance
    = Debug
    | Info
    | Error
    deriving (Eq, Ord, Show)

{- | Represents a structured logging event

The event holds a given importance, and then an arbitrary collection
of key-value pairs.
-}
data Event = Event !Importance !UTCTime ![(Text, Text)]
    deriving (Eq)

instance Ord Event where
    (Event ia _ _) <= (Event ib _ _) = ia <= ib


infixr 8 .=
(.=) :: Show v => Text -> v -> (Text, Text)
a .= b = (a, show b)


-- | The information a logger needs
data LoggerInfo = LoggerInfo
    { loggerIEvents :: !(TBQueue Event)
    -- | The seperation text between events
    , loggerISep :: !Text
    -- | Whether or not to log the time
    , loggerITime :: !Bool
    }

-- | Represents a computation that can log things
newtype LoggerM a = LoggerM (ReaderT LoggerInfo IO a)
    deriving (Functor, Applicative, Monad,
              MonadReader LoggerInfo, MonadIO
             )

-- | Start a loop for the logger, where it will repeatedly log events
loggerLoop :: LoggerM ()
loggerLoop = forever $ do
    info  <- ask
    event <- atomically $ readTBQueue (loggerIEvents info)
    putText (makeLog info event)
  where
    makeLog :: LoggerInfo -> Event -> Text
    makeLog info (Event i time pairs) =
        let sep        = loggerISep info
            eventTexts = map (\(a, b) -> a <> ": " <> b) pairs
            realTime   = if loggerITime info then " " <> show time else ""
            header     = mconcat ["[", show i, realTime, "]"]
        in  Text.intercalate sep (header : eventTexts) <> "\n"


-- | Configuration for the logger
data LoggerConfig = LoggerConfig
    { loggerSep :: !Text -- | The seperation between elements
    -- | Whether or not to log the time
    , loggerTime :: !Bool
    -- | The size of the event buffer to keep
    , loggerBufSize :: !Int
    }

-- | A default value for the logger configuration
defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = LoggerConfig ", " True 1024

{- | Start a logger with a given config

This will launch the logger in a thread that will die if the surrounding one does.
-}
startLogger :: LoggerConfig -> IO (Async (), LoggerHandle)
startLogger LoggerConfig {..} = do
    q <- newTBQueueIO (fromIntegral loggerBufSize)
    let info           = LoggerInfo q loggerSep loggerTime
        (LoggerM loop) = loggerLoop
    pid <- async $ runReaderT loop info
    return (pid, LoggerHandle q)


-- | A handle allowing us to send messages to a logger
newtype LoggerHandle = LoggerHandle (TBQueue Event)

class HasLogger m where
    getLogger :: m LoggerHandle

-- | Log an event to the logger
log :: (MonadIO m, HasLogger m) => Importance -> [(Text, Text)] -> m ()
log i pairs = do
    time <- liftIO getCurrentTime
    let event = Event i time pairs
    (LoggerHandle q) <- getLogger
    atomically $ writeTBQueue q event
