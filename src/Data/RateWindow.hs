{-# LANGUAGE PatternSynonyms #-}
{- |
Description: contains functions for calculating download rates.

This is mainly useful for calculating download rates for a peer.
We can measure rates on a connection by looking at how many bytes we received
in a packet, and the time elapsed since the last packet. The problem is that this
"point-wise" estimate is prone to large fluctuations, so we want a smoothing
mechanism. By using a sliding window of these data points, we can average them
to get a better estimate of the rate.
-}
module Data.RateWindow
    ( RateWindow
    , makeRateWindow
    , ByteCount
    , addRatePoint
    , getRate
    )
where

import           Relude

import           Data.Sequence                  ( (|>)
                                                , Seq(..)
                                                )
import qualified Data.Sequence                 as Seq
import qualified Data.Time.Clock               as Time


-- | ByteCount is an Int, counting the number of bytes received over an interval
type ByteCount = Int

{- | RateWindow allows us to keep a sliding window of download rates

We can insert point intervals of downloads, and then average them out,
in order to get a more accurate point of view on the download rate.

The main data flow for this type should be first constructing it
with 'makeRateWindow' then subsequently using 'addRatePoint' each time
a packet is received, and 'getRate' to get the current rate estimate.
-}
data RateWindow = RateWindow Int (Seq.Seq (ByteCount, Time.NominalDiffTime))

-- | Construct a RateWindow given a window size
makeRateWindow :: Int -> RateWindow
makeRateWindow maxSize = RateWindow maxSize Empty

-- | Take off the oldest timestamp if the rate window has too many elements
removeOldest :: RateWindow -> RateWindow
removeOldest (RateWindow maxSize sq) =
    let sq' = if Seq.length sq >= maxSize then rmFront sq else sq
    in  RateWindow maxSize sq'
  where
    rmFront :: Seq.Seq a -> Seq.Seq a
    rmFront Empty        = Empty
    rmFront (_ :<| rest) = rest

-- | Add a single download point to the rate window
addRatePoint :: ByteCount -> Time.NominalDiffTime -> RateWindow -> RateWindow
addRatePoint count delta (RateWindow maxSize sq) =
    removeOldest $ RateWindow maxSize (sq |> (count, delta))

{- | Get the full average from the series of point stamps

This returns (total bytes receieved) / (total seconds elapsed), giving
a much smoother download rate compared to each tcp response.
-}
getRate :: RateWindow -> Double
getRate (RateWindow _ sq) =
    let add (acc1, acc2) (a, b) = (acc1 + a, acc2 + b)
        (count, delta) = foldl' add (0, 0) sq
    in  fromIntegral count / fromRational (toRational delta)
