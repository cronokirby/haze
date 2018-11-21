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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Network.HTTP.Client
import qualified Network.Socket as Sock
import Network.Socket.ByteString (sendTo, sendAllTo, recv)


import Haze.Bencoding (DecodeError(..))
import Haze.Bits (encodeIntegralN)
import Haze.Tracker (Tracker(..), MetaInfo(..), metaFromBytes,
                     newTrackerRequest, 
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
    case metaAnnounce torrent of
        HTTPTracker url -> connectHTTP torrent url
        UDPTracker url port -> 
            Sock.withSocketsDo $
            connectUDP torrent url port

connectHTTP :: MetaInfo -> Text -> IO ()
connectHTTP torrent url = do
    mgr <- newManager defaultManagerSettings
    request <- parseRequest (toString url)
    peerID <- generatePeerID
    let trackerReq = newTrackerRequest torrent peerID
        query = trackerQuery trackerReq
    let withQuery = setQueryString query request
    response <- httpLbs withQuery mgr
    let bytes = LBS.toStrict $ responseBody response
        announce = announceFromHTTP bytes
    print announce


-- | Connect to a UDP tracker with url and port
connectUDP :: MetaInfo -> Text -> Text -> IO ()
connectUDP torrent url port = do
    putTextLn (url <> ":" <> port)
    let urlS = toString url
        portS = toString port
        hints = Just Sock.defaultHints
            { Sock.addrSocketType = Sock.Datagram }
    target:_ <- Sock.getAddrInfo Nothing (Just urlS) (Just portS)
    let fam  = Sock.addrFamily target
        addr = (Sock.addrAddress target)
    sock <- Sock.socket fam Sock.Datagram Sock.defaultProtocol
    connect sock addr
    data1 <- recv sock 1024
    putBSLn data1
  where
    connect :: Sock.Socket -> Sock.SockAddr -> IO ()
    connect sock addr = do
        sendAllTo sock bytes addr
      where
        magic :: Int64 
        magic = 4497486125440
        bytes = BS.pack (encodeIntegralN 8 magic)
             <> "\0\0\0\0"
             <> "\20\34\34\100"
