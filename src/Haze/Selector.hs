{-# LANGUAGE  GeneralizedNewtypeDeriving #-}
{- |
Description: contains the component that chokes and unchokes peers

This module contains the component responsible for deciding
which peers we should reciprocate to, and which ones
we shouldn't. It's necessary to have a single component
responsible for making these decisions.
-}
module Haze.Selector
    ()
where

import           Relude

import           Control.Concurrent.STM.TBQueue ( writeTBQueue )
import           Data.Maybe                     ( fromJust )
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import           Data.Time.Clock                ( getCurrentTime )

import           Data.RateWindow                ( RateWindow
                                                , getRate
                                                )
import           Haze.Messaging                 ( ManagerToPeer(..) )
import           Haze.PeerInfo                  ( PeerInfo(..)
                                                , PeerSpecific(..)
                                                )
import           Haze.Tracker                   ( Peer )


-- | The information a Selector needs
data SelectorInfo = SelectorInfo
    { selectorPeerInfo :: !PeerInfo -- | Information on peers
    -- | The set of peers we've chosen to download to
    , selectorDownloaders :: !(HS.HashSet Peer)
    {- | The set of peers we'd like to download from us

    A peer can join this set either because it was chosen
    at randomly optimistically, or because it had a better
    download rate.
    -}
    , selectorUninterested :: !(HS.HashSet Peer)
    }

newtype SelectorM a = SelectorM (ReaderT SelectorInfo IO a)
    deriving (Functor, Applicative, Monad,
              MonadReader SelectorInfo, MonadIO)


{- | This will select 4 peers to unchoke.

This should be run every 10 seconds or so.
-}
selectPeers :: SelectorM ()
selectPeers = do
    peerMap   <- asks (infoMap . selectorPeerInfo) >>= readTVarIO
    peerRates <- forM (HM.toList peerMap) $ \(peer, spec) -> do
        rate <- extractRate (peerDLRate spec)
        return (peer, rate)
    let bestPeers = take 4 . map fst $ sortBy (flip compare `on` snd) peerRates
    atomically . forM_ bestPeers $ \peer -> do
        let spec = fromJust $ HM.lookup peer peerMap
        writeTBQueue (peerFromManager spec) PeerIsWorthy
  where
    extractRate :: MonadIO m => TVar RateWindow -> m Double
    extractRate var = do
        now <- liftIO getCurrentTime
        atomically $ do
            val <- readTVar var
            let (newVal, rate) = getRate now 10 val
            newVal `seq` writeTVar var newVal
            return rate
