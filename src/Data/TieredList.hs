{- |
Description: Contains functions and utilities for TieredLists
-}
module Data.TieredList
    ( TieredList
    , makeTieredList
    )
where

import Relude


-- | Represents a list of tiers of equal priority
newtype TieredList a = TieredList [[a]] 
    deriving (Eq, Show)

-- | Make a Tiered list from a list of tiers
makeTieredList :: [[a]] -> TieredList a
makeTieredList = TieredList
