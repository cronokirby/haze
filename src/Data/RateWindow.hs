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

import           Data.Sequence                  ( (<|)
                                                , Seq(..)
                                                , dropWhileR
                                                )
import qualified Data.Time.Clock               as Time


-- | ByteCount is an Int, counting the number of bytes received over an interval
type ByteCount = Int

{- | Represents a sliding time window, allowing us to calculate average rates

The flow for using this type is to start off with
'emptyRateWindow', then use 'addDownload' in a monotonic way,
until eventually calculating the recent rate with 'getRate',
which will clean up old download stamps.
-}
newtype RateWindow = RateWindow (Seq (ByteCount, Time.UTCTime))

-- | an empty rate window
emptyRateWindow :: RateWindow
emptyRateWindow = RateWindow Empty

{- | Get the download rate in a certain period from the current time.

The first argument is the point from which to search back
and calculate an average. The second is the window over which to look.

This will clean up timestamps that are past this interval,
so this is intended to be used with a consistent interval.
-}
getRate
    :: Time.UTCTime
    -> Time.NominalDiffTime
    -> RateWindow
    -> (RateWindow, Double)
getRate now size (RateWindow dls) =
    let isOld time = Time.diffUTCTime now time > size
        current = dropWhileR (\(_, time) -> isOld time) dls
        rate    = case current of
            Empty -> 0.0
            (_ :|> (_, oldest)) ->
                let fullCount = sum (fmap fst current)
                    window    = Time.diffUTCTime now oldest
                in  calcRate fullCount window
    in  (RateWindow current, rate)
  where
    calcRate :: ByteCount -> Time.NominalDiffTime -> Double
    calcRate count diff = fromRational (fromIntegral count / toRational diff)

{- | Add a new data point to the rate window.

Note that the functions for this structure assume that
the time for data points we add will only increase.
-}
addDownload :: ByteCount -> Time.UTCTime -> RateWindow -> RateWindow
addDownload count time (RateWindow sq) = RateWindow ((count, time) <| sq)
