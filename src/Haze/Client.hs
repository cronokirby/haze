{- | 
Description: contains functions for interacting with a tracker

This exports IO actions for ineracting with a tracker, including
connecting and maintaining communication lines.
-}
module Haze.Client
    (launchClient
    )
where

import Relude

import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Client

import Haze.Bencoding (DecodeError(..))
import Haze.Tracker (MetaInfo(..), metaFromBytes, newTrackerRequest, 
                    trackerQuery, announceFromHTTP)


launchClient :: FilePath -> IO ()
launchClient file = do
    bytes <- readFileBS file
    case metaFromBytes bytes of
        Left (DecodeError err) -> do
            putStrLn "Failed to decode file:"
            putTextLn err
        Right meta -> launchTorrent meta


launchTorrent :: MetaInfo -> IO ()
launchTorrent torrent = do
    mgr <- newManager defaultManagerSettings
    request <- parseRequest (toString (metaAnnounce torrent))
    let withQuery = setQueryString query request
    response <- httpLbs withQuery mgr
    let bytes = LBS.toStrict $ responseBody response
        announce = announceFromHTTP bytes
    print announce
  where
    trackerReq = newTrackerRequest torrent "HZ123456789123456789"
    query = trackerQuery trackerReq
