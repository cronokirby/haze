{-|
Description: Functions related to the Bencoding type

Contains the base data type representing Bencoding
data, as well as encoders and decoders to marshall
this data from byte strings.

ByteString is used as the base target, since Bencoding
isn't guaranteed to be in utf-8 text, however the strings
contained inside the format are.
-}
module Haze.Bencoding
    ( Bencoding(..)
    , Encoder(..)
    , encodeBen
    , encode
    , Decoder(..)
    , decodeBen
    , decode
    )
where

import Relude

import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.HashMap.Strict as HM
import Data.Int (Int64)


{- | Represents Bencoded data.

Bencoded strings can be arbitrary byte sequences, and aren't
always guaranteed to be valid text.

The show instance just shows the raw data structure,
the encode the structure as a sequence of bytes,
look to the other encoding functions instead.
-}
data Bencoding
    = BString !ByteString
    | BInt !Int64
    | BList ![Bencoding]
    | BMap !(HM.HashMap ByteString Bencoding)
    deriving (Eq, Show)


-- | Represents the encoding of some type into Bencoding
newtype Encoder a = Encoder 
    { runEncoder :: a -> Bencoding 
    }


{- | Encode Bencoding as itself.

This is useful for combining with 'encode'.
-}
encodeBen :: Encoder Bencoding
encodeBen = Encoder id


-- | Encode a thing as a bytestring.
encode :: Encoder a -> a -> ByteString
encode encoder = encodeBS . runEncoder encoder
  where
    encodeBS :: Bencoding -> ByteString
    encodeBS (BString t) = 
        show (BS.length t) <> ":" <> t
    encodeBS (BInt i)    =
        "i" <> show i <> "e"
    encodeBS (BList bs)  =
        "l" <> foldMap encodeBS bs <> "e"
    encodeBS (BMap mp)   =
        "d" <> foldMap encodeKeyPair (toSorted mp) <> "e"
    toSorted :: Ord k => HM.HashMap k v -> [(k, v)]
    toSorted = sortWith fst . HM.toList
    encodeKeyPair :: (ByteString, Bencoding) -> ByteString
    encodeKeyPair (k, v) = 
        encodeBS (BString k) <> encodeBS v


    
-- | Represents Bencoding decoding errors for a bytestring
newtype DecodeError = DecodeError Text deriving (Eq, Show)

-- | Represents the decoding of some Bencoding structure into a type
newtype Decoder a = Decoder
    { runDecoder :: Bencoding -> a
    }


{- | Decode Bencoding as itself. 

This is useful for combining with 'decode'.
-}
decodeBen :: Decoder Bencoding
decodeBen = Decoder id


-- | Decode a bytestring into something
decode :: Decoder a -> ByteString -> Either DecodeError a
decode (Decoder d) = fmap d . makeDecoderError . AP.parseOnly parse
  where
    makeDecoderError = either (Left . DecodeError . toText) Right
    parse :: AP.Parser Bencoding
    parse = parseInt
        <|> (BString <$> parseString)
        <|> parseList
        <|> parseMap
    parseInt :: AP.Parser Bencoding
    parseInt = do
        _   <- AP.char 'i'
        int <- signedInt
        _   <- AP.char 'e'
        return (BInt int)
      where
        signedInt :: AP.Parser Int64
        signedInt = (negate <$> (AP.char '-' *> AP.decimal))
                <|> AP.decimal
    parseString :: AP.Parser ByteString
    parseString = do
        len    <- AP.decimal
        _      <- AP.char ':'
        AP.take len
    parseList :: AP.Parser Bencoding
    parseList = do
        _  <- AP.char 'l'
        xs <- AP.many' parse
        _  <- AP.char 'e'
        return (BList xs)
    parseMap :: AP.Parser Bencoding
    parseMap = do
        _     <- AP.char 'd'
        pairs <- AP.many' parsePair
        _     <- AP.char 'e'
        return . BMap . HM.fromList $ pairs
    parsePair :: AP.Parser (ByteString, Bencoding)
    parsePair = do
        k <- parseString
        v <- parse
        return (k, v)
