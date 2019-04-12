{- | The Announcer is responsible for getting information from Trackers.

The main way of interacting with this component is by listening
to the 'AnnounceInfo' messages it produces. Because we only
listen to the announces as they arrive, we can be agnostic to changes
in the underlying tracker, for example.
-}
module Announcer
    ()
where

import           Relude

import           Control.Concurrent.STM.TBQueue ( TBQueue )

import           Data.TieredList                ( TieredList
                                                , popTiered
                                                )
import           Haze.Tracker                   ( AnnounceInfo
                                                , MetaInfo
                                                , Tracker
                                                )


-- | Represents an error that can interrupt interaction with a Tracker
data AnnounceError 
    -- | We couldn't parse a tracker's response
    = AnnounceFailedParse !Text
    -- | The tracker sent us a warning
    | AnnounceWarning !Text
    -- | The tracker had a mismatched transaction id
    | AnnounceBadTransaction
    deriving (Show)

-- | Represents the results of an announce with a Tracker
data AnnounceResult 
    -- | We had no issues in getting an Announce
    = GoodAnnounce !AnnounceInfo
    {- | Some error happened during the Announce

    This also indicates the death of the scout connected to that Tracker.
    -}
    | BadAnnounce !AnnounceError
    deriving (Show)

-- | Represents all the information an Announcer needs.
data AnnouncerInfo = AnnouncerInfo
    { announcerTorrent :: !MetaInfo
    -- | An 'MVar' is sufficient to receive 'AnnounceInfo' from our scout
    , announcerMsg :: !(MVar AnnounceResult)
    {- | This holds a list of Trackers we want to try and connect to.

    Only the thread launching scouts has access to this, so an IORef
    is fine.
    -}
    , announcerTrackers :: !(IORef (TieredList Tracker))
    -- | This allows us to report back successful announces
    , announcerResults :: !(TBQueue AnnounceInfo)
    }


