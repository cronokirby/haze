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
import           Haze.Messaging                 ( SelectorToPeer(..) )
import           Haze.PeerInfo                  ( PeerInfo(..)
                                                , PeerSpecific(..)
                                                , PeerFriendship(..)
                                                )
import           Haze.Tracker                   ( Peer )


-- | The information a Selector needs
data SelectorInfo = SelectorInfo
    { selectorPeerInfo :: !PeerInfo -- | Information on peers
    -- | The set of peers we've chosen to download to
    , selectorDownloaders :: !(TVar (HS.HashSet Peer))
    {- | The set of peers we'd like to download from us

    A peer can join this set either because it was chosen
    at randomly optimistically, or because it had a better
    download rate.
    -}
    , selectorUninterested :: !(TVar (HS.HashSet Peer))
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
    let sortedPeers = sortBy (flip compare `on` snd) peerRates
        isInterested peer = do
            let spec = fromJust $ HM.lookup peer peerMap
            friendship <- readTVarIO $ peerFriendship spec
            return (peerIsInterested friendship)
    interestedPeers <- filterM (isInterested . fst) sortedPeers
    let bestRate = fromMaybe 0.0 . viaNonEmpty head $ map snd interestedPeers
        newDownloaders = HS.fromList . map fst $ take 4 interestedPeers
        better =
            HS.fromList . map fst $ takeWhile ((> bestRate) . snd) sortedPeers
        newWatched = HS.difference better newDownloaders
    downloaders  <- asks selectorDownloaders
    uninterested <- asks selectorUninterested
    atomically $ do
        writeTVar downloaders  newDownloaders
        writeTVar uninterested newWatched
        forM_ newDownloaders (sendToPeer peerMap PeerUnchoke)
        forM_ newWatched     (sendToPeer peerMap PeerWatchForInterest)
  where
    sendToPeer
        :: HM.HashMap Peer PeerSpecific -> SelectorToPeer -> Peer -> STM ()
    sendToPeer mp msg peer = do
        let q = peerFromSelector . fromJust $ HM.lookup peer mp
        writeTBQueue q msg
    extractRate :: MonadIO m => TVar RateWindow -> m Double
    extractRate var = do
        now <- liftIO getCurrentTime
        atomically $ do
            val <- readTVar var
            let (newVal, rate) = getRate now 10 val
            newVal `seq` writeTVar var newVal
            return rate
