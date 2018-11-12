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
    )
where

import Relude

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Int (Int64)


{- | Represents Bencoded data.

Note that although Bencoded data is stored as bytes,
each string in the format is valid ASCII, and thus is
represented as Text.

The show instance just shows the raw data structure,
the encode the structure as a sequence of bytes,
look to the other encoding functions instead.
-}
data Bencoding
    = BString !Text
    | BInt !Int64
    | BList ![Bencoding]
    | BMap !(HM.HashMap Text Bencoding)
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
        show (T.length t) <> ":" <> encodeUtf8 t
    encodeBS (BInt i)    =
        "i" <> show i <> "e"
    encodeBS (BList bs)  =
        "l" <> foldMap encodeBS bs <> "e"
    encodeBS (BMap mp)   =
        "d" <> foldMap encodeKeyPair (HM.toList mp) <> "e"
    encodeKeyPair :: (Text, Bencoding) -> ByteString
    encodeKeyPair (k, v) = 
        encodeBS (BString k) <> encodeBS v
