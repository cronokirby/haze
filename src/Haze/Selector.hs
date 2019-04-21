{-# LANGUAGE  GeneralizedNewtypeDeriving #-}
{-# LANGUAGE  NumericUnderscores         #-}
{-# LANGUAGE  RecordWildCards            #-}
{- |
Description: contains the component that chokes and unchokes peers

This module contains the component responsible for deciding
which peers we should reciprocate to, and which ones
we shouldn't. It's necessary to have a single component
responsible for making these decisions.
-}
module Haze.Selector
    ( SelectorInfo
    , makeSelectorInfo
    , SelectorM
    , runSelectorM
    , selectorLoop
    )
where

import           Relude

import           Control.Concurrent             ( threadDelay )
import           Control.Concurrent.Async       ( withAsync )
import           Control.Concurrent.STM.TBQueue ( readTBQueue
                                                , writeTBQueue
                                                )
import           Data.List                      ( (!!) )
import           Data.Maybe                     ( fromJust )
import qualified Data.HashMap.Strict           as HM
import qualified Data.HashSet                  as HS
import           Data.Time.Clock                ( UTCTime
                                                , getCurrentTime
                                                )
import           System.Random                  ( randomRIO )

import           Control.Logger                 ( HasLogger(..)
                                                , LoggerHandle
                                                , Importance(..)
                                                , (.=)
                                                , log
                                                )
import           Data.RateWindow                ( RateWindow
                                                , getRate
                                                )
import           Haze.Messaging                 ( PeerToSelector(..)
                                                , SelectorToPeer(..)
                                                )
import           Haze.PeerInfo                  ( PeerInfo(..)
                                                , PeerSpecific(..)
                                                , PeerFriendship(..)
                                                )
import           Haze.Tracker                   ( Peer )


-- | The information a Selector needs
data SelectorInfo = SelectorInfo
    { selectorPeerInfo :: !PeerInfo -- | Information on peers
    -- | The handle to the logger
    , selectorLogger :: !LoggerHandle
    -- | The set of peers we've chosen to download to
    , selectorDownloaders :: !(TVar (HS.HashSet Peer))
    {- | The set of peers we'd like to download from us

    A peer can join this set either because it was chosen
    at randomly optimistically, or because it had a better
    download rate.
    -}
    , selectorUninterested :: !(TVar (HS.HashSet Peer))
    }

-- | Construct the information a Selector needs
makeSelectorInfo :: MonadIO m => PeerInfo -> LoggerHandle -> m SelectorInfo
makeSelectorInfo peerInfo lh =
    SelectorInfo peerInfo lh <$> newTVarIO HS.empty <*> newTVarIO HS.empty

newtype SelectorM a = SelectorM (ReaderT SelectorInfo IO a)
    deriving (Functor, Applicative, Monad,
              MonadReader SelectorInfo, MonadIO)

instance HasLogger SelectorM where
    getLogger = asks selectorLogger

-- | Run a selector computation given the right information
runSelectorM :: SelectorM a -> SelectorInfo -> IO a
runSelectorM (SelectorM m) = runReaderT m

-- | Log information with the source as the selector
logSelector :: Importance -> [(Text, Text)] -> SelectorM ()
logSelector i pairs = log i ("source" .= ("selector" :: String) : pairs)


{- | This starts the selector process

This will select new peers every 10 seconds, optimistically
select a new peer every 30, and listen to message any time.
-}
selectorLoop :: SelectorM ()
selectorLoop = do
    logSelector Info ["msg" .= ("started" :: String)]
    info <- ask
    let peerAction   = runSelectorM fromPeerLoop info
        selectAction = runSelectorM (selectLoop 0) info
    liftIO $ withAsync peerAction (const selectAction)
  where
    fromPeerLoop :: SelectorM ()
    fromPeerLoop = forever $ do
        q   <- asks (infoToSelector . selectorPeerInfo)
        msg <- atomically $ readTBQueue q
        reactToPeer msg
    selectLoop :: Int -> SelectorM ()
    selectLoop n = do
        downloaders <- readTVarIO =<< asks selectorDownloaders
        logSelector Debug ["downloaders" .= HS.toList downloaders]
        liftIO $ threadDelay 10_000_000
        selectPeers
        if n == 2
            then do
                selectOptimistically
                selectLoop 0
            else selectLoop (n + 1)

extractRate :: TVar RateWindow -> UTCTime -> STM Double
extractRate var now = do
    val <- readTVar var
    let (newVal, rate) = getRate now 10 val
    newVal `seq` writeTVar var newVal
    return rate

sendToPeer :: HM.HashMap Peer PeerSpecific -> SelectorToPeer -> Peer -> STM ()
sendToPeer mp msg peer = do
    let q = peerFromSelector . fromJust $ HM.lookup peer mp
    writeTBQueue q msg


{- | This will select 4 peers to unchoke.

This should be run every 10 seconds or so.

This chooses the 4 peers that are interested in us, and have the best
downloaded rates, and then notifies those peers of unchoking.
We also watch peers with a better download rate than these 4, and
if they become interested, we boot our lowest of the 4 peers.
-}
selectPeers :: SelectorM ()
selectPeers = do
    peerMap   <- asks (infoMap . selectorPeerInfo) >>= readTVarIO
    now       <- liftIO getCurrentTime
    peerRates <- forM (HM.toList peerMap) $ \(peer, spec) -> do
        rate <- atomically $ extractRate (peerDLRate spec) now
        return (peer, rate)
    let sortedPeers = sortBy (flip compare `on` snd) peerRates
        isInterested peer = do
            let spec = fromJust $ HM.lookup peer peerMap
            friendship <- readTVarIO $ peerFriendship spec
            return (peerIsInterested friendship)
    logSelector Noisy ["sorted-peers" .= sortedPeers]
    interestedPeers <- filterM (isInterested . fst) sortedPeers
    let bestRate = fromMaybe 0.0 . viaNonEmpty head $ map snd interestedPeers
        newDownloaders = HS.fromList . map fst $ take 4 interestedPeers
        better =
            HS.fromList . map fst $ takeWhile ((> bestRate) . snd) sortedPeers
        newWatched = HS.difference better newDownloaders
    logSelector Noisy ["new-downloaders" .= newDownloaders]
    logSelector Noisy ["new-watched" .= newWatched]
    downloaders  <- asks selectorDownloaders
    uninterested <- asks selectorUninterested
    atomically $ do
        writeTVar downloaders  newDownloaders
        writeTVar uninterested newWatched
        forM_ newDownloaders (sendToPeer peerMap PeerUnchoke)
        forM_ newWatched     (sendToPeer peerMap PeerWatchForInterest)


{- | Select a new peer to unchoke optimistically.

Once that peer becomes interested, we kick our lowest performing downloader.

This new peer is chosen at random among the peers that we haven't alread
unchoked, or started watching.

This should be called every 3rd select round, in addition to the normal
selection process.
-}
selectOptimistically :: SelectorM ()
selectOptimistically = do
    dldrs   <- readTVarIO =<< asks selectorDownloaders
    watched <- readTVarIO =<< asks selectorUninterested
    peerMap <- readTVarIO =<< asks (infoMap . selectorPeerInfo)
    let allPeers = HS.fromList $ HM.keys peerMap
        toSelect = HS.difference allPeers (HS.union dldrs watched)
    chosen <- chooseRandom toSelect
    newDownloader chosen
  where
    chooseRandom :: MonadIO m => HS.HashSet a -> m a
    chooseRandom set = do
        let size = HS.size set
        r <- liftIO $ randomRIO (0, size - 1)
        return (HS.toList set !! r)

-- | Handle a peer's message to us
reactToPeer :: PeerToSelector -> SelectorM ()
reactToPeer (PeerBecameInterested peer) = newDownloader peer

{- | Replace the worst downloader with a new peer.

This is useful when a previously selected peer
becomes interested. In that case, we want to replace our
worst downloader with it.
-}
newDownloader :: Peer -> SelectorM ()
newDownloader peer = do
    SelectorInfo {..} <- ask
    let PeerInfo {..} = selectorPeerInfo
    now <- liftIO getCurrentTime
    atomically $ do
        peerMap     <- readTVar infoMap
        downloaders <- readTVar selectorDownloaders
        watched     <- readTVar selectorUninterested
        if HS.size downloaders < 4
            then do
                let newDownloaders = HS.insert peer downloaders
                    newWatched     = HS.delete peer watched
                writeTVar selectorDownloaders  newDownloaders
                writeTVar selectorUninterested newWatched
            else do
                rates <- forM (HS.toList downloaders) $ \pr -> do
                    let spec = fromJust $ HM.lookup peer peerMap
                    rate <- extractRate (peerDLRate spec) now
                    return (pr, rate)
                let worst =
                        fst . fromJust . viaNonEmpty head $ sortOn snd rates
                    newDownloaders =
                        downloaders & HS.delete worst & HS.insert peer
                    newWatched = HS.delete peer watched
                sendToPeer peerMap PeerChoke worst
                writeTVar selectorDownloaders  newDownloaders
                writeTVar selectorUninterested newWatched
