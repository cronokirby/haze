{-# LANGUAGE RecordWildCards #-}
{- |
Description: Contains functions related to trackers

This file provides a more abstract description of
the communication protocol with trackers. First it
specificies the data in a .torrent file with MetaInfo,
then data sent to and returned from a tracker.
-}
module Haze.Tracker
    ( Tracker(..)
    , TieredList
    , MD5Sum(..)
    , SHA1(..)
    , SHAPieces(..)
    , FileInfo(..)
    , FileItem(..)
    , totalFileLength
    , MetaInfo(..)
    , totalFileSize
    , squashedTrackers
    , decodeMeta
    , metaFromBytes
    , UDPConnection(..)
    , parseUDPConn
    , UDPTrackerRequest
    , newUDPRequest
    , updateUDPTransID
    , updateUDPConnID
    , encodeUDPRequest
    , Announce(..)
    , AnnounceInfo(..)
    , parseUDPAnnounce
    , PeerID(..)
    , peerIDBytes
    , generatePeerID
    , Peer(..)
    , decodeAnnounce
    , announceFromHTTP
    , ReqEvent(..)
    , TrackStatus(..)
    , firstTrackStatus
    , updateTrackStatus
    , updateUDPTrackStatus
    , TrackerRequest(..)
    , newTrackerRequest
    , updateTransactionID
    , trackerQuery
    )
where

import           Relude

import qualified Crypto.Hash.SHA1              as SHA1
import qualified Data.Attoparsec.ByteString    as AP
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as BSC
import           Data.Hashable                  ( Hashable(..) )
import qualified Data.HashMap.Strict           as HM
import qualified Data.Text                     as T
import           Data.Time.Clock                ( DiffTime
                                                , UTCTime
                                                , getCurrentTime
                                                , utctDayTime
                                                )
import           Data.Time.Clock.POSIX          ( posixSecondsToUTCTime )
import           Network.Socket                 ( HostName
                                                , PortNumber
                                                )
import           Path                           ( Path
                                                , Rel
                                                , File
                                                , Dir
                                                , (</>)
                                                )
import qualified Path
import           Text.Show                      ( Show(..) )

import           Data.TieredList                ( TieredList
                                                , makeTieredList
                                                , tieredSingleton
                                                )
import           Haze.Bencoding                 ( Bencoding(..)
                                                , Decoder(..)
                                                , DecodeError(..)
                                                , decode
                                                , encode
                                                , encodeBen
                                                )
import           Haze.Bits                      ( Bits
                                                , encodeIntegralN
                                                , packBytes
                                                , parseInt
                                                )


{- | Represents the URL for a torrent Tracker

This distinguishes between the different types of
supported clients. UDPTracker comes with a pre split
link and port, ready for socket connection.
-}
data Tracker
    = HTTPTracker !Text
    | UDPTracker !Text !Text
    | UnknownTracker !Text
    deriving (Show)


{- | Try and get the type of tracker from a URL

Makes a decision based on the presence of udp:// or
http:// or https:// in the url. 
Will fail completely if none of these is found.
-}
trackerFromURL :: Text -> Tracker
trackerFromURL t | T.isPrefixOf "udp://" t   = udpFromURL t
                 | T.isPrefixOf "http://" t  = HTTPTracker t
                 | T.isPrefixOf "https://" t = HTTPTracker t
                 | otherwise                 = UnknownTracker t
  where
    udpFromURL t' = fromMaybe (UnknownTracker t) $ do
        unPrefix <- T.stripPrefix "udp://" t'
        let (url, port) = T.span (/= ':') unPrefix
        return (UDPTracker url (T.drop 1 port))


-- | Represents the MD5 sum of a file
newtype MD5Sum = MD5Sum ByteString deriving (Show)

-- | Represents a 20 byte SHA1 hash
newtype SHA1 = SHA1 { getSHA1 :: ByteString } deriving (Eq, Show)

{- | Represents the concatenation of multiple SHA pieces.

The integer represents the length of each piece, and the bytestring
the concatenation of all the SHA1 hashes
-}
data SHAPieces = SHAPieces Int64 ByteString deriving (Eq)

instance Show SHAPieces where
    show (SHAPieces i _) = "SHAPieces " ++ Relude.show i ++ " (..bytes)"

{- | Represents the information in the `info` of a metainfo file

A torrent can contain either a single file, or multiple files,
and what each file contains in the multi file mode is different than
the single file.
-}
data FileInfo
    -- | A single file, with name, length, and md5 sum
    = SingleFile !FileItem
    -- | Multiple files, with directory name
    | MultiFile !(Path Rel Dir) ![FileItem]
    deriving (Show)

-- | Returns the total length of all files in the torrent
totalFileLength :: FileInfo -> Int64
totalFileLength fileInfo = case fileInfo of
    SingleFile item   -> itemLength item
    MultiFile _ items -> sum $ map itemLength items
  where
    itemLength :: FileItem -> Int64
    itemLength (FileItem _ l _) = l

{- | A single file in a multi file torrent

The Raw representation is a list of paths, but we concatenate
and verify the validity of those as an actual relative file path
during parsing. For example ["dir", "file.ext"] will become "dir/file.ext"
-}
data FileItem = FileItem !(Path Rel File) !Int64 !(Maybe MD5Sum) deriving (Show)


{- | Represents the information in a .torrent file

Contains information about the files contained in the
torrent, and the trackers to use to connect to peers
seeding those files.
-}
data MetaInfo = MetaInfo
    { metaPieces :: !SHAPieces
    , metaPrivate :: !Bool
    , metaFile :: !FileInfo
    , metaInfoHash :: !SHA1
    , metaAnnounce :: !Tracker
    , metaAnnounceList :: !(Maybe (TieredList Tracker))
    , metaCreation :: !(Maybe UTCTime)
    , metaComment :: !(Maybe Text)
    , metaCreatedBy :: !(Maybe Text)
    , metaEncoding :: !(Maybe Text)
    }
    deriving (Show)

{- | Make a tiered list of trackers no matter what

If the announce list isn't present, there will be a single
tier with just the given trackers. If the tracker list
is present, the single tracker is ignored.
-}
squashedTrackers :: MetaInfo -> TieredList Tracker
squashedTrackers MetaInfo {..} =
    fromMaybe (tieredSingleton metaAnnounce) metaAnnounceList


-- | Try and decode a meta file from a bytestring
metaFromBytes :: ByteString -> Either DecodeError MetaInfo
metaFromBytes bs =
    decode decodeMeta bs
        >>= maybe (Left (DecodeError "Bad MetaInfo file")) Right

-- | Get the total size (bytes) of all the files in a torrent
totalFileSize :: MetaInfo -> Int64
totalFileSize meta = totalFileLength $ metaFile meta


type BenMap = HM.HashMap ByteString Bencoding

decodeMeta :: Decoder (Maybe MetaInfo)
decodeMeta = Decoder doDecode
  where
    doDecode (BMap mp) = do
        info <- HM.lookup "info" mp
        (metaPieces, metaPrivate, metaFile) <- getInfo info
        let metaInfoHash = SHA1 $ SHA1.hash (encode encodeBen info)
        announceURL <- withKey "announce" mp tryText
        let metaAnnounce     = trackerFromURL announceURL
        let metaAnnounceList = getAnnounces "announce-list" mp
        let metaCreation     = withKey "creation date" mp tryDate
        let metaComment      = withKey "comment" mp tryText
        let metaCreatedBy    = withKey "created by" mp tryText
        let metaEncoding     = withKey "encoding" mp tryText
        return (MetaInfo { .. })
    doDecode _ = Nothing
    getBool :: ByteString -> BenMap -> Bool
    getBool k mp = case HM.lookup k mp of
        Just (BInt 1) -> True
        _             -> False
    getAnnounces :: ByteString -> BenMap -> Maybe (TieredList Tracker)
    getAnnounces k mp = makeTieredList
        <$> withKey k mp (traverse getTrackers <=< tryList)
      where
        getTrackers :: Bencoding -> Maybe [Tracker]
        getTrackers = traverse (fmap trackerFromURL . tryText) <=< tryList
    tryDate :: Bencoding -> Maybe UTCTime
    tryDate (BInt i) = Just . posixSecondsToUTCTime $ fromInteger (toInteger i)
    tryDate _        = Nothing
    getInfo :: Bencoding -> Maybe (SHAPieces, Bool, FileInfo)
    getInfo (BMap mp) = do
        let private = getBool "private" mp
        pieceLen  <- withKey "piece length" mp tryInt
        pieceHash <- withKey "pieces" mp tryBS
        let sha = SHAPieces pieceLen pieceHash
        file <- case HM.lookup "files" mp of
            Nothing    -> getSingle mp
            Just files -> getMulti mp files
        return (sha, private, file)
    getInfo _ = Nothing
    getFilePart :: BenMap -> Maybe (Int64, Maybe MD5Sum)
    getFilePart mp = do
        len <- withKey "length" mp tryInt
        let md5 = MD5Sum <$> withKey "md5sum" mp tryBS
        return (len, md5)
    getSingle :: BenMap -> Maybe FileInfo
    getSingle mp = do
        name       <- withKey "name" mp tryPath
        (len, md5) <- getFilePart mp
        path       <- Path.parseRelFile name
        return (SingleFile (FileItem path len md5))
    getMulti :: BenMap -> Bencoding -> Maybe FileInfo
    getMulti mp (BList l) = do
        name  <- withKey "name" mp tryPath
        files <- traverse getFileItem l
        dir   <- Path.parseRelDir name
        return (MultiFile dir files)
    getMulti _ _ = Nothing
    getFileItem :: Bencoding -> Maybe FileItem
    getFileItem (BMap mp) = do
        (len, md5) <- getFilePart mp
        rawParts   <- withKey "path" mp tryList
        strings    <- traverse tryPath rawParts >>= nonEmpty
        dirs       <- traverse Path.parseRelDir (init strings)
        file       <- Path.parseRelFile (last strings)
        let joinedPath = foldr (</>) file dirs
        return (FileItem joinedPath len md5)
    getFileItem _ = Nothing


-- | Information sent to the tracker about the state of the request
data ReqEvent
    -- | The request has just started
    = ReqStarted
    -- | The request has stopped
    | ReqStopped
    -- | The request has successfully downloaded everything
    | ReqCompleted
    -- | No new information about the request
    | ReqEmpty
    deriving (Show)

{- | Represents information about the health of the request.

The tracker is interested in this information, as well as the user.
-}
data TrackStatus = TrackStatus
    { trackUp :: !Int64 -- | The total number of bytes uploaded
    -- | The total number of bytes downloaded
    , trackDown :: !Int64
    -- | the total number of bytes in the file left to download
    , trackLeft :: !Int64
    }
    deriving (Show)

-- | Create the first track status given the torrent file
firstTrackStatus :: MetaInfo -> TrackStatus
firstTrackStatus meta = TrackStatus 0 0 (totalFileSize meta)

-- | Represents the information in a request to a tracker
data TrackerRequest = TrackerRequest
    { treqInfoHash :: !SHA1
    -- | Represents the peer id for this client
    , treqPeerID :: !ByteString
    , treqPort :: !PortNumber
    , treqStatus :: !TrackStatus
    -- | Whether or not the client expects a compact response
    , treqCompact :: !Bool
    -- | The current state of this ongoing request
    , treqEvent :: !ReqEvent
    , treqNumWant :: !(Maybe Int)
    -- | This is to be included if the tracker sent it
    , treqTransactionID :: !(Maybe ByteString)
    }
    deriving (Show)

-- | Constructs the tracker request to be used at the start of a session
newTrackerRequest :: MetaInfo -> ByteString -> TrackerRequest
newTrackerRequest meta@MetaInfo {..} peerID = TrackerRequest
    metaInfoHash
    peerID
    6881
    (firstTrackStatus meta)
    True
    ReqStarted
    Nothing
    Nothing

updateTransactionID :: Maybe ByteString -> TrackerRequest -> TrackerRequest
updateTransactionID transID treq = 
    treq { treqTransactionID = transID, treqEvent = ReqEmpty }

-- | Update the tracking status of a request
updateTrackStatus :: TrackStatus -> TrackerRequest -> TrackerRequest
updateTrackStatus status treq = treq { treqStatus = status }


-- | Encodes a 'TrackerRequest' as query parameters
trackerQuery :: TrackerRequest -> [(ByteString, Maybe ByteString)]
trackerQuery TrackerRequest {..} =
    map (\(a, b) -> (a, Just b))
        $  [ ("info_hash" , getSHA1 treqInfoHash)
           , ("peer_id"   , treqPeerID)
           , ("port"      , Relude.show treqPort)
           , ("uploaded"  , Relude.show (trackUp treqStatus))
           , ("downloaded", Relude.show (trackDown treqStatus))
           , ("left"      , Relude.show (trackLeft treqStatus))
           , ("compact"   , if treqCompact then "1" else "0")
           ]
        ++ eventQuery
        ++ maybe [] (\i -> [("numwant", Relude.show i)]) treqNumWant
        ++ maybe [] (\s -> [("trackerid", s)])           treqTransactionID
  where
    eventQuery = case treqEvent of
        ReqStarted   -> [("event", "started")]
        ReqStopped   -> [("event", "stopped")]
        ReqCompleted -> [("event", "completed")]
        ReqEmpty     -> []


{- | A UDP tracker will send this after a connection

Contains a transaction ID and a connection ID
-}
data UDPConnection = UDPConnection ByteString ByteString

parseUDPConn :: AP.Parser UDPConnection
parseUDPConn = do
    _     <- AP.string "\0\0\0\0"
    trans <- AP.take 4
    conn  <- AP.take 8
    return (UDPConnection trans conn)

-- | Represents a request to a UDP tracker
data UDPTrackerRequest =
    UDPTrackerRequest ByteString TrackerRequest

-- | Construct a new UDP request.
newUDPRequest :: MetaInfo -> ByteString -> UDPConnection -> UDPTrackerRequest
newUDPRequest meta peerID (UDPConnection trans conn) =
    let trackerReq = newTrackerRequest meta peerID
        withTrans  = trackerReq { treqTransactionID = Just trans }
    in  UDPTrackerRequest conn withTrans

-- | Updates the transaction ID in a UDP request
updateUDPTransID :: ByteString -> UDPTrackerRequest -> UDPTrackerRequest
updateUDPTransID transID (UDPTrackerRequest c treq) =
    UDPTrackerRequest c (updateTransactionID (Just transID) treq)

-- | Updates the connection ID in a UDP request
updateUDPConnID :: ByteString -> UDPTrackerRequest -> UDPTrackerRequest
updateUDPConnID connID (UDPTrackerRequest _ treq) =
    UDPTrackerRequest connID treq

-- | Update the tracking status of a UDP request
updateUDPTrackStatus :: TrackStatus -> UDPTrackerRequest -> UDPTrackerRequest
updateUDPTrackStatus status (UDPTrackerRequest c treq) =
    UDPTrackerRequest c (updateTrackStatus status treq)

-- | Encodes a UDP request as a bytestring
encodeUDPRequest :: UDPTrackerRequest -> ByteString
encodeUDPRequest (UDPTrackerRequest conn TrackerRequest {..}) =
    conn
        <> "\0\0\0\1"
    -- The upstream tracker won't like this
        <> fromMaybe "\0\0\0\0" treqTransactionID
        <> getSHA1 treqInfoHash
        <> treqPeerID
        <> pack64 (trackUp treqStatus)
        <> pack64 (trackDown treqStatus)
        <> pack64 (trackLeft treqStatus)
        <> pack32 eventNum
    -- The IP address we hardcode (default)
        <> "\0\0\0\0"
    -- This should be sufficiently unique
        <> BS.drop 16 treqPeerID
        <> pack32 (fromMaybe (-1) treqNumWant)
        <> packPort treqPort
  where
    pack64 :: Int64 -> ByteString
    pack64 = BS.pack . encodeIntegralN 8
    pack32 :: (Bits i, Integral i) => i -> ByteString
    pack32 = BS.pack . encodeIntegralN 4
    packPort :: PortNumber -> ByteString
    packPort p = BS.drop 2 (pack32 ((fromIntegral p) :: Int))
    eventNum :: Int32
    eventNum = case treqEvent of
        ReqEmpty     -> 0
        ReqCompleted -> 1
        ReqStarted   -> 2
        ReqStopped   -> 3


-- | Represents the announce response from a tracker
data Announce
    -- | The request to the tracker was bad
    = FailedAnnounce !Text
    | GoodAnnounce !AnnounceInfo
    deriving (Show)

-- | The information of a successful announce response
data AnnounceInfo = AnnounceInfo
    { annWarning :: !(Maybe Text) -- ^ A warning message
    , annInterval :: !Int -- ^ Seconds between requests
    -- | If present, the client must not act more frequently
    , annMinInterval :: !(Maybe Int)
    , annTransactionID :: !(Maybe ByteString)
    -- | The number of peers with the complete file
    , annSeeders :: !(Maybe Int)
    -- | The number of peers without the complete file
    , annLeechers :: !(Maybe Int)
    , annPeers :: ![Peer]
    }
    deriving (Show)


-- | Represents an identifier we share with other peers
newtype PeerID = PeerID ByteString deriving (Eq, Show)

-- | Get the bytestring form of a peer id
peerIDBytes :: PeerID -> ByteString
peerIDBytes (PeerID bytes) = bytes

{- | Generates a peer id from scratch.

Note that this should be generated before the first interaction with
a tracker, and not at every interaction with the tracker.

Uses the Azureus style id, with HZ as the prefix, and then appends
a UTC timestamp, before then taking only the first 20 bytes.
-}
generatePeerID :: MonadIO m => m PeerID
generatePeerID = liftIO $ do
    secs <- getSeconds
    let whole = "-HZ010-" <> Relude.show secs
        cut   = BS.take 20 whole
    return (PeerID cut)
  where
    getSeconds :: MonadIO m => m DiffTime
    getSeconds = liftIO $ utctDayTime <$> getCurrentTime


{- | Represents a peer in the swarm

A Peer can be hashed, which will use it's peerID,
if it has one, and the host name.
-}
data Peer = Peer
    { peerID :: !(Maybe PeerID)
    , peerHost :: !HostName
    , peerPort :: !PortNumber
    }
    deriving (Show)

instance Eq Peer where
    (Peer idA hostA _) == (Peer idB hostB _) = idA == idB && hostA == hostB

instance Hashable Peer where
    hashWithSalt salt (Peer peerID host _) =
        salt `hashWithSalt` (peerIDBytes <$> peerID) `hashWithSalt` host

{- | This reads a bytestring announce from HTTP

HTTP and UDP trackers differ in that HTTP trackers
will send back a bencoded bytestring to read the
announce information from, but UDP trackers will
send a bytestring without bencoding.
This parses the bencoded bytestring from HTTP.
-}
announceFromHTTP :: ByteString -> Either DecodeError Announce
announceFromHTTP bs =
    decode decodeAnnounce bs
        >>= maybe (Left (DecodeError "Bad Announce Data")) Right


-- | Decode a bytestring as a list of Peer addresses
decodeBinaryPeers :: ByteString -> Maybe [Peer]
decodeBinaryPeers bs | BS.length bs `mod` 6 /= 0 = Nothing
                     | otherwise = Just . map makeHostAndPort $ makeChunks 6 bs
  where
    makeChunks :: Int -> ByteString -> [ByteString]
    makeChunks size body
        | BS.null body = []
        | otherwise    = BS.take size body : makeChunks size (BS.drop size body)
    makePeerHost :: ByteString -> String
    makePeerHost chunk =
        intercalate "." . map Relude.show $ BS.unpack (BS.take 4 chunk)
    makePeerPort :: ByteString -> PortNumber
    makePeerPort chunk =
        -- this is safe because of when we call this
        packBytes (BS.unpack (BS.drop 4 chunk))
    makeHostAndPort :: ByteString -> Peer
    makeHostAndPort chnk = Peer Nothing (makePeerHost chnk) (makePeerPort chnk)

-- | Parse Announce information from a UDP tracker
parseUDPAnnounce :: AP.Parser Announce
parseUDPAnnounce = do
    _                <- AP.string "\0\0\0\1"
    annTransactionID <- Just <$> AP.take 4
    let annWarning = Nothing
    annInterval <- parseInt
    let annMinInterval = Nothing
    annLeechers <- Just <$> parseInt
    annSeeders  <- Just <$> parseInt
    rest        <- AP.takeByteString
    case decodeBinaryPeers rest of
        Nothing       -> fail "Failed to decode binary peers"
        Just annPeers -> return (GoodAnnounce AnnounceInfo { .. })

-- | A Bencoding decoder for the Announce data
decodeAnnounce :: Decoder (Maybe Announce)
decodeAnnounce = Decoder doDecode
  where
    doDecode :: Bencoding -> Maybe Announce
    doDecode (BMap mp) = case HM.lookup "failure reason" mp of
        Just (BString s) -> Just (FailedAnnounce (decodeUtf8 s))
        Nothing          -> do
            info <- decodeAnnounceInfo mp
            return (GoodAnnounce info)
        Just _ -> Nothing
    doDecode _ = Nothing
    decodeAnnounceInfo :: BenMap -> Maybe AnnounceInfo
    decodeAnnounceInfo mp = do
        let annWarning = withKey "warning message" mp tryText
        annInterval <- withKey "interval" mp tryNum
        let annMinInterval   = withKey "min interval" mp tryNum
        let annTransactionID = withKey "tracker id" mp tryBS
        let annSeeders       = withKey "complete" mp tryNum
        let annLeechers      = withKey "incomplete" mp tryNum
        pInfo    <- HM.lookup "peers" mp
        annPeers <- dictPeers pInfo <|> binPeers pInfo
        return (AnnounceInfo { .. })
    dictPeers :: Bencoding -> Maybe [Peer]
    dictPeers = tryList >=> traverse getPeer
      where
        getPeer :: Bencoding -> Maybe Peer
        getPeer (BMap mp) = do
            let peerID = PeerID <$> withKey "peer id" mp tryBS
            peerHost <- BSC.unpack <$> withKey "ip" mp tryBS
            peerPort <- withKey "port" mp tryNum
            return (Peer { .. })
        getPeer _ = Nothing
    binPeers :: Bencoding -> Maybe [Peer]
    binPeers (BString bs) = decodeBinaryPeers bs
    binPeers _            = Nothing

{- Decoding utilities -}

withKey :: ByteString -> BenMap -> (Bencoding -> Maybe a) -> Maybe a
withKey k mp next = HM.lookup k mp >>= next

tryInt :: Bencoding -> Maybe Int64
tryInt (BInt i) = Just i
tryInt _        = Nothing

tryNum :: Num n => Bencoding -> Maybe n
tryNum (BInt i) = Just (fromInteger (toInteger i))
tryNum _        = Nothing

tryBS :: Bencoding -> Maybe ByteString
tryBS (BString bs) = Just bs
tryBS _            = Nothing

tryPath :: Bencoding -> Maybe FilePath
tryPath = fmap BSC.unpack . tryBS

tryText :: Bencoding -> Maybe Text
tryText = fmap decodeUtf8 . tryBS

tryList :: Bencoding -> Maybe [Bencoding]
tryList (BList l) = Just l
tryList _         = Nothing
