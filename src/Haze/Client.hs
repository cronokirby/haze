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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Network.HTTP.Client

import Haze.Bencoding (DecodeError(..))
import Haze.Tracker (MetaInfo(..), metaFromBytes, newTrackerRequest, 
                    trackerQuery, announceFromHTTP)


{- | Generates a peer id from scratch.

Note that this should be generated before the first interaction with
a tracker, and not at every interaction with the tracker.

Uses the Azureus style id, with HZ as the prefix, and then appends
a UTC timestamp, before then taking only the first 20 bytes.
-}
generatePeerID :: IO ByteString
generatePeerID = do
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
        Right meta -> launchTorrent meta


launchTorrent :: MetaInfo -> IO ()
launchTorrent torrent = do
    mgr <- newManager defaultManagerSettings
    request <- parseRequest (toString (metaAnnounce torrent))
    peerID <- generatePeerID
    let trackerReq = newTrackerRequest torrent peerID
        query = trackerQuery trackerReq
    let withQuery = setQueryString query request
    response <- httpLbs withQuery mgr
    let bytes = LBS.toStrict $ responseBody response
        announce = announceFromHTTP bytes
    print announce
