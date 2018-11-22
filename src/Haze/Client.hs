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

import Control.Exception.Safe (bracket)
import Data.Attoparsec.ByteString as AP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Network.HTTP.Client
import qualified Network.Socket as Sock
import Network.Socket.ByteString (sendAllTo, recv)


import Haze.Bencoding (DecodeError(..))
import Haze.Tracker (Tracker(..), MetaInfo(..), metaFromBytes,
                     newTrackerRequest, 
                     trackerQuery, announceFromHTTP,
                     parseUDPConn, parseUDPAnnounce,
                     newUDPRequest, encodeUDPRequest)


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
    peerID <- generatePeerID
    case metaAnnounce torrent of
        HTTPTracker url -> connectHTTP peerID torrent url
        UDPTracker url prt -> 
            Sock.withSocketsDo $
            connectUDP peerID torrent url prt

connectHTTP :: ByteString -> MetaInfo -> Text -> IO ()
connectHTTP peerID torrent url = do
    mgr <- newManager defaultManagerSettings
    request <- parseRequest (toString url)
    let trackerReq = newTrackerRequest torrent peerID
        query = trackerQuery trackerReq
    let withQuery = setQueryString query request
    response <- httpLbs withQuery mgr
    let bytes = LBS.toStrict $ responseBody response
        announce = announceFromHTTP bytes
    print announce


-- | Represents a UDP connection to some tracker
data UDPSocket = UDPSocket Sock.Socket Sock.SockAddr


-- | Connect to a UDP tracker with url and port
connectUDP :: ByteString -> MetaInfo -> Text -> Text -> IO ()
connectUDP peerID torrent url prt = do
    bracket (makeUDPSocket url prt) closeUDPSocket $ \udp -> do
        initiate udp
        connBytes <- recvUDP udp 1024
        connInfo <- parseFail parseUDPConn connBytes
        let request = newUDPRequest torrent peerID connInfo
        sendUDP udp (encodeUDPRequest request)
        annBytes <- recvUDP udp 1024
        announce <- parseFail parseUDPAnnounce annBytes
        print announce
  where
    makeUDPSocket :: Text -> Text -> IO UDPSocket
    makeUDPSocket url prt = do
        let urlS =  toString url
            portS = toString prt
            hints = Just Sock.defaultHints
                { Sock.addrSocketType = Sock.Datagram 
                }
        target:_ <- Sock.getAddrInfo hints (Just urlS) (Just portS)
        let fam  = Sock.addrFamily target
            addr = Sock.addrAddress target
        sock <- Sock.socket fam Sock.Datagram Sock.defaultProtocol
        return (UDPSocket sock addr)
    closeUDPSocket :: UDPSocket -> IO ()
    closeUDPSocket (UDPSocket sock _) = Sock.close sock
    recvUDP :: UDPSocket -> Int -> IO ByteString
    recvUDP (UDPSocket sock _) amount = recv sock amount
    sendUDP :: UDPSocket -> ByteString -> IO ()
    sendUDP (UDPSocket sock addr ) bytes = 
        sendAllTo sock bytes addr
    initiate :: UDPSocket -> IO ()
    initiate udp = sendUDP udp $
      "\0\0\4\x17\x27\x10\x19\x80"
      <> "\0\0\0\0"
      -- This should be sufficiently unique
      <> BS.drop 16 peerID

parseFail :: MonadFail m => AP.Parser a -> ByteString -> m a
parseFail parser bs =
    case AP.parseOnly parser bs of
        Left s  -> fail s
        Right a -> return a
