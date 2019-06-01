{- |
Description: Contains functions related to configuring the application

This contains the definition for the config type we use to share
the variables that change the behavior of the application,
as well as utilities for parsing that config from the command line.
-}
module Haze.Config
    ( Config(..)
    , Port(..)
    , parseConfig
    )
where

import           Relude

import           Path                           ( Path
                                                , Abs
                                                , File
                                                , Dir
                                                , (</>)
                                                )
import qualified Path
import qualified Path.IO                       as Path

import           Options.Applicative


-- | Represents all the configuration data we need
data Config = Config
    { configTorrentFile :: !(Path Abs File)
    -- | This will be the current directory if no argument is parsed
    , configDownloadDir :: !(Path Abs Dir)
    {- | The file to log to

    If this is nothing, then we shouldn't log
    at all.
    -}
    , configLogFile :: !(Maybe (Path Abs File))
    -- | The port to listen or incoming connections on.
    , configPort :: !Port
    }
    deriving (Show)

newtype Port = Port Int deriving (Show, Read)

-- | Read an absolute directory given a root for relative directories
rootedDir :: Path Abs Dir -> ReadM (Path Abs Dir)
rootedDir root = either id (root </>) <$> (absDir <|> relDir)
  where
    absDir = Left <$> maybeReader Path.parseAbsDir
    relDir = Right <$> maybeReader Path.parseRelDir

-- | Read an absolute file given a root for relative files
rootedFile :: Path Abs Dir -> ReadM (Path Abs File)
rootedFile root = either id (root </>) <$> (absFile <|> relFile)
  where
    absFile = Left <$> maybeReader Path.parseAbsFile
    relFile = Right <$> maybeReader Path.parseRelFile


configParser :: Path Abs Dir -> Parser Config
configParser root =
    Config
        <$> argument
                (rootedFile root)
                (metavar "TORRENT_FILE" <> help "The torrent file to download")
        <*> option
                (rootedDir root)
                (  metavar "DIRECTORY"
                <> long "output-dir"
                <> short 'o'
                <> help "The directory to download the torrent to"
                <> value root
                )
        <*> option
                (Just <$> rootedFile root)
                (  metavar "LOG_FILE"
                <> long "log-file"
                <> short 'l'
                <> help "Logging will happen to this file if set"
                <> value Nothing
                )
        <*> (Port <$>
              option
                auto
                (  metavar "PORT"
                <> long "port"
                <> short 'p'
                <> help "The port to listen for incoming connections on"
                <> value 6881
                ))

version :: Parser (a -> a)
version = infoOption
    "Haze version 0.1.1"
    (long "version" <> short 'v' <> help
        "Display what version the program is using"
    )

argsParser :: Path Abs Dir -> ParserInfo Config
argsParser root = info
    (configParser root <**> helper <**> version)
    (fullDesc <> progDesc "Download the torrent in TORRENT_FILE")

-- | Parse a configuration from the command line
parseConfig :: MonadIO m => m Config
parseConfig = do
    root <- Path.getCurrentDir
    liftIO $ execParser (argsParser root)
