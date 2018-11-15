{-# LANGUAGE RecordWildCards #-}
{- |
Description: Contains functions for parsing .torrent files

Each .torrent file is a MetaInfo file, containing
information about the files in the torrent. This module
exports data structure and functions to read and operate
on these files.
-}
module Haze.MetaInfo 
    ( Tracker(..)
    , TieredList(..)
    , MD5Sum(..)
    , SHAPieces(..)
    , FileInfo(..)
    , FileItem(..)
    , MetaInfo(..)
    , decodeMeta
    , metaFromBytes
    )
where

import Relude

import qualified Data.HashMap.Strict as HM
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Haze.Bencoding (Bencoding(..), Decoder(..),
                       decode, DecodeError(..))


-- | Represents the URL for a torrent Tracker
newtype Tracker = Tracker Text deriving (Show)

{- | Represents a tiered list of objects.

Every element in the tier is tried before moving on
to the next tier. In MetaInfo files, multiple
tiers of trackers are provided, with each tier needing
to be tried before the subsequent one is used.
-}
data TieredList a = TieredList [[a]] deriving (Show)

-- | Represents the MD5 sum of a file
newtype MD5Sum = MD5Sum ByteString deriving (Show)

-- | Represents the concatenation of multiple SHA pieces.
data SHAPieces = SHAPieces Int64 ByteString deriving (Show)

{- | Represents the information in the `info` of a metainfo file

A torrent can contain either a single file, or multiple files,
and what each file contains in the multi file mode is different than
the single file.
-}
data FileInfo 
    -- | A single file, with name, length, and md5 sum
    = SingleFile Text Int64 (Maybe MD5Sum)
    -- | Multiple files, with directory name
    |  MultiFile Text [FileItem]
    deriving (Show)

{- | A single file in a multi file torrent

Note that the information in this datatype is slightly different
from the 'SingleFile' branch of 'FileInfo'. Notably, instead of
having a name, it instead has a list of strings representing
the full file path, which must be respected.
-}
data FileItem = FileItem [Text] Int64 (Maybe MD5Sum) deriving (Show)

{- | Represents the information in a .torrent file
stem.Directory
Contains information about the files contained in the
torrent, and the trackers to use to connect to peers
seeding those files.
-}
data MetaInfo = MetaInfo
    { metaPieces :: SHAPieces
    , metaPrivate :: Bool
    , metaFile :: FileInfo
    , metaAnnounce :: Text
    , metaAnnounceList :: TieredList Tracker 
    , metaCreation :: Maybe UTCTime
    , metaComment :: Maybe Text
    , metaCreatedBy :: Maybe Text 
    , metaEncoding :: Maybe Text
    }
    deriving (Show)


-- | Try and decode a meta file from a bytestring
metaFromBytes :: ByteString -> Either DecodeError MetaInfo
metaFromBytes bs = decode decodeMeta bs 
    >>= maybe (Left (DecodeError "Bad MetaInfo file")) Right


type BenMap = HM.HashMap ByteString Bencoding

decodeMeta :: Decoder (Maybe MetaInfo)
decodeMeta = Decoder decode
  where
    decode (BMap mp) = do
        info <- HM.lookup "info" mp
        (metaPieces, metaPrivate, metaFile) <- getInfo info
        metaAnnounce     <- withKey "announce" mp tryText
        metaAnnounceList <- getAnnounces "announce-list" mp
        let metaCreation  = withKey "creation date" mp tryDate
        let metaComment   = withKey "comment" mp tryText
        let metaCreatedBy = withKey "created by" mp tryText
        let metaEncoding  = withKey "encoding" mp tryText
        return (MetaInfo {..})
    decode _          = Nothing
    withKey :: ByteString -> BenMap 
               -> (Bencoding -> Maybe a) -> Maybe a
    withKey k mp next = HM.lookup k mp >>= next
    getBool :: ByteString -> BenMap -> Bool
    getBool k mp = case HM.lookup k mp of
        Just (BInt 1) -> True
        _             -> False
    tryInt :: Bencoding -> Maybe Int64
    tryInt (BInt i) = Just i
    tryInt _        = Nothing
    tryBS :: Bencoding -> Maybe ByteString
    tryBS (BString bs) = Just bs
    tryBS _            = Nothing
    tryText :: Bencoding -> Maybe Text
    tryText = fmap decodeUtf8 . tryBS
    tryList :: Bencoding -> Maybe [Bencoding]
    tryList (BList l) = Just l
    tryList _         = Nothing
    getAnnounces :: ByteString -> BenMap -> Maybe (TieredList Tracker)
    getAnnounces k mp = 
        withKey k mp 
        (fmap TieredList . traverse getTrackers <=< tryList)
      where
        getTrackers :: Bencoding -> Maybe [Tracker]
        getTrackers = 
            traverse (fmap Tracker . tryText) <=< tryList
    tryDate :: Bencoding -> Maybe UTCTime
    tryDate (BInt i) = Just . posixSecondsToUTCTime $
            fromInteger (toInteger i)
    tryDate _        = Nothing
    getInfo :: Bencoding -> Maybe (SHAPieces, Bool, FileInfo)
    getInfo (BMap mp) = do
        let private = getBool "private" mp
        pieceLen <- withKey "piece length" mp tryInt
        pieces   <- withKey "pieces" mp tryBS
        let sha = SHAPieces pieceLen pieces
        file <- case HM.lookup "files" mp of
            Nothing    -> getSingle mp
            Just files -> getMulti mp files
        return (sha, private, file)
    getInfo _         = Nothing
    getFilePart :: BenMap -> Maybe (Int64, Maybe MD5Sum)
    getFilePart mp = do
        len <- withKey "length" mp tryInt
        let md5 = MD5Sum <$> withKey "md5sum" mp tryBS
        return (len, md5)
    getSingle :: BenMap -> Maybe FileInfo
    getSingle mp = do
        name <- withKey "name" mp tryText
        (len, md5) <- getFilePart mp
        return (SingleFile name len md5)
    getMulti :: BenMap -> Bencoding -> Maybe FileInfo
    getMulti mp (BList l) = do
        name  <- withKey "name" mp tryText
        files <- traverse getFileItem l
        return (MultiFile name files)
    getMulti _ _         = Nothing
    getFileItem :: Bencoding -> Maybe FileItem
    getFileItem (BMap mp) = do
        (len, md5) <- getFilePart mp
        path <- withKey "path" mp 
                (traverse tryText <=< tryList)
        return (FileItem path len md5)
    getFileItem _         = Nothing
