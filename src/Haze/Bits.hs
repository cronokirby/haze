{- |
Description: Contains utility functions for working with bits.
-}
module Haze.Bits 
    ( Bits
    , encodeIntegralN
    , packBytes
    )
where

import Relude

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
