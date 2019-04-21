{-# LANGUAGE PatternSynonyms #-}
{- |
Description: contains functions for calculating download rates.

This is mainly useful for calculating download rates for a peer.
We can measure rates on a connection by looking at how many bytes we received
in a packet, and the time elapsed since the last packet. The problem is that this
"point-wise" estimate is prone to large fluctuations, so we want a smoothing
mechanism. By using a sliding window of these data points, we can look at how many
bytes were downloaded in the last time frame.
-}
module Data.RateWindow
    ( RateWindow
    , emptyRateWindow
    , getRate
    , addDownload
    )
where

import           Relude


-- | ByteCount is an Int, counting the number of bytes received over an interval
type ByteCount = Int

{- | Represents a sliding time window, allowing us to calculate average rates

The flow for using this type is to start off with
'emptyRateWindow', then use 'addDownload' in a monotonic way,
until eventually calculating the recent rate with 'getRate',
which will clean up old download stamps.
-}
newtype RateWindow = RateWindow ByteCount

-- | an empty rate window
emptyRateWindow :: RateWindow
emptyRateWindow = RateWindow 0

-- | Get the download rate over the period since we last checked
getRate :: RateWindow -> (RateWindow, ByteCount)
getRate (RateWindow count) = (RateWindow 0, count)

{- | Add a new data point to the rate window.

Note that the functions for this structure assume that
the time for data points we add will only increase.
-}
addDownload :: ByteCount -> RateWindow -> RateWindow
addDownload count (RateWindow fullCount) = RateWindow $! count + fullCount
