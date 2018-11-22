{- |
Description: Contains functions and utilities for TieredLists
-}
module Data.TieredList
    ( TieredList
    , makeTieredList
    , tieredSingleton
    , popTiered
    )
where

import Relude


-- | Represents a list of tiers of equal priority
newtype TieredList a = TieredList [[a]] 
    deriving (Eq, Show)

-- | Make a Tiered list from a list of tiers
makeTieredList :: [[a]] -> TieredList a
makeTieredList = TieredList

-- | Make a tiered list with a single item
tieredSingleton :: a -> TieredList a
tieredSingleton a = makeTieredList [[a]]


{- | Pop off the highest priority element from a tiered list.

Returns Nothing if the list is null
-}
popTiered :: TieredList a -> Maybe (a, TieredList a)
popTiered (TieredList []) = 
    Nothing
popTiered (TieredList ([]:rest)) = 
    popTiered (TieredList rest)
popTiered (TieredList ((x:xs):rest)) =
    Just (x, TieredList (xs:rest))
    