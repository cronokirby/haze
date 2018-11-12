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
    (
    )
where

import Relude

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HM
import Data.Word (Word64)


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
    | BInt !Word64
    | BList ![Bencoding]
    | BMap !(HM.HashMap Text Bencoding)
    deriving (Eq, Show)
