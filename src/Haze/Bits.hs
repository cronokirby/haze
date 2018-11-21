{- |
Description: Contains utility functions for working with bits.
-}
module Haze.Bits 
    ( Bits
    , encodeIntegralN
    , packBytes
    , parseInt
    , parse16
    )
where

import Relude

import qualified Data.Attoparsec.ByteString as AP
import Data.Bits (Bits, (.|.), (.&.), shiftL, shiftR)


-- | Encodes an integral number as a list of N bytes
encodeIntegralN :: (Bits i, Integral i) => Int -> i -> [Word8]
encodeIntegralN n i =
    let push x acc = fromIntegral (x .&. 255) : acc
        go _ (x, acc) = (shiftR x 8, push x acc)
    in snd $ foldr go (i, []) (take n (cycle [()]))

-- | Fold a list of Bytes (big endian) into a number
packBytes :: Num n => [Word8] -> n
packBytes = 
    fromIntegral
    . foldr go 0
    . reverse
  where
    go :: Word8 -> Int -> Int
    go b acc = shiftL acc 8 .|. fromIntegral b


-- | Parse 4 big endian bytes as an Int
parseInt :: AP.Parser Int
parseInt = do
    b1 <- AP.anyWord8
    b2 <- AP.anyWord8 
    b3 <- AP.anyWord8
    b4 <- AP.anyWord8
    return $ packBytes [b1, b2, b3, b4]

-- | Parse 2 big endian bytes as a Number
parse16 :: Num n => AP.Parser n
parse16 = do
    a <- AP.anyWord8
    b <- AP.anyWord8
    return $ packBytes [a, b]

