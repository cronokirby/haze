{-# LANGUAGE RecordWildCards #-}
{- |
Description: Contains functions around keeping information on peers

We spawn threads for peers to act autonomously, but we need to be
able to communicate across channels to these peers. We need to be able
to keep a map of peers. We can add to this map as we connect,
and remove from this map as peers close their connections.

We also want to keep track of certain statistics about the peers,
such as their current download rate, and the sets of pieces they have.
-}
module Haze.PeerInfo
    ( PeerFriendship(..)
    , PeerHandle(..)
    , PeerInfo(..)
    , makeEmptyPeerInfo
    , HasPeerInfo(..)
    , addPeer
    , sendWriterToPeer
    , sendWriterToAll
    , recvToWriter
    )
where

import           Relude

import           Control.Concurrent.STM.TBQueue ( TBQueue
                                                , newTBQueueIO
                                                , readTBQueue
                                                , writeTBQueue
                                                )
import           Data.Array                     ( Array )
import qualified Data.HashMap.Strict           as HM
import qualified Data.Set                      as Set

import           Data.RateWindow                ( RateWindow
                                                , emptyRateWindow
                                                )
import           Haze.Messaging                 ( PeerToWriter(..)
                                                , WriterToPeer(..)
                                                , ManagerToPeer(..)
                                                )
import           Haze.PieceBuffer               ( PieceBuffer
                                                , bufferArr
                                                , makePieceBuffer
                                                )
import           Haze.Tracker                   ( MetaInfo
                                                , Peer
                                                , TrackStatus
                                                , firstTrackStatus
                                                )


{- | Holds information on our relationship with a peer

This needs to be exposed in order to make decisions on which peers
to unchoke based on whether or not they are interested in what we have.
-}
data PeerFriendship = PeerFriendship
    { peerIsChoking :: !Bool -- | Whether or not they are choking us
    -- | Whether or not I am choking them
    , peerAmChoking :: !Bool
    -- | Whether or not the peer is interested in me
    , peerIsInterested :: !Bool
    -- | Whether or not we're interested in them
    , peerAmInterested :: !Bool
    }

{- | The default state for our friendship

At the beginning of our relationship with the peer, neither us nor
them are interested in anything the other has to offer, and neither
of us are letting the other download anything.
-}
emptyFriendship :: PeerFriendship
emptyFriendship = PeerFriendship True True False False


{- | A peer handle contains the information a peer shares with the rest of us.

After adding a peer to the map, we return this handle so they can share
information with everybody else.
-}
data PeerHandle = PeerHandle
    { handlePieces :: !(Array Int (TVar Int)) -- ^ piece index -> count
    -- | The pieces we currently have
    , handleOurPieces :: !(TVar (Set Int))
    -- | The piece buffer we share with everyone
    , handleBuffer :: !(TVar PieceBuffer)
    -- | The out bound message queue to the writer
    , handleToWriter :: !(TBQueue PeerToWriter)
    -- | The specific channel from the writer
    , handleFromWriter :: !(TBQueue WriterToPeer)
    -- | The specific channel from the manager
    , handleFromManager :: !(TBQueue ManagerToPeer)
    -- | The relationship with that peer
    , handleFriendship :: !(TVar PeerFriendship)
    -- | The rate window for downloading
    , handleDLRate :: !(TVar RateWindow)
    -- | The status of the download rates
    , handleStatus :: !(TVar TrackStatus)
    -- | The peer associated with this handle
    , handlePeer :: !Peer
    }

{- | PeerSpecific holds the information owned by one peer only.

As opposed to general structures like the piece rarity map,
each peer has its own communication channels, as well as other things.
To handle this, we have this struct for specific information
-}
data PeerSpecific = PeerSpecific
    { peerFromWriter :: !(TBQueue WriterToPeer) -- ^ a queue from the writer
    -- | A queue to allow the manager to send us messages
    , peerFromManager :: !(TBQueue ManagerToPeer)
    -- | The friendship for our peer
    , peerFriendship :: !(TVar PeerFriendship)
    -- | The download rate window for this peer
    , peerDLRate :: !(TVar RateWindow)
    }

-- | Create a new empty struct of PeerSpecific Data
makePeerSpecific :: MonadIO m => m PeerSpecific
makePeerSpecific =
    PeerSpecific
        <$> mkQueue
        <*> mkQueue
        <*> newTVarIO emptyFriendship
        <*> newTVarIO emptyRateWindow
    where mkQueue = liftIO (newTBQueueIO 256)

{- | This holds general information about the operation of the peers.

Specifically, it contains a mapping from each Peer to the specific
information that they need. 
-}
data PeerInfo = PeerInfo
    { infoPieces :: !(Array Int (TVar Int)) -- ^ piece index -> count
    -- | The pieces we currently have
    , infoOurPieces :: !(TVar (Set Int))
    -- | The shared piece buffer
    , infoBuffer :: !(TVar PieceBuffer)
    -- | The shared message queue to the writer
    , infoToWriter :: !(TBQueue PeerToWriter)
    -- | The information about our upload and download status
    , infoStatus :: !(TVar TrackStatus)
    -- | A map from a Peer to specific Peer data
    , infoMap :: !(TVar (HM.HashMap Peer PeerSpecific))
    }

-- | This creates the initial Peer information
makeEmptyPeerInfo :: MonadIO m => MetaInfo -> m PeerInfo
makeEmptyPeerInfo meta = do
    let buffer = makePieceBuffer 0x4000 meta
        makeCountVar _ = newTVarIO (0 :: Int)
    infoPieces    <- traverse makeCountVar $ bufferArr buffer
    infoOurPieces <- newTVarIO Set.empty
    infoBuffer    <- newTVarIO buffer
    infoToWriter  <- liftIO $ newTBQueueIO 1024
    infoStatus    <- newTVarIO (firstTrackStatus meta)
    infoMap       <- newTVarIO HM.empty
    return PeerInfo { .. }

-- | Represents a class of contexts in which we have access to pieceinfo
class HasPeerInfo m where
    getPeerInfo :: m PeerInfo

-- | Make a handle from specific and shared information
makeHandle :: PeerSpecific -> PeerInfo -> Peer -> PeerHandle
makeHandle PeerSpecific {..} PeerInfo {..} = PeerHandle infoPieces
                                                        infoOurPieces
                                                        infoBuffer
                                                        infoToWriter
                                                        peerFromWriter
                                                        peerFromManager
                                                        peerFriendship
                                                        peerDLRate
                                                        infoStatus

-- | Add a new peer to the information we have
addPeer :: MonadIO m => Peer -> PeerInfo -> m PeerHandle
addPeer newPeer info = do
    let mapVar = infoMap info
    newVal <- makePeerSpecific
    atomically $ modifyTVar' mapVar (HM.insert newPeer newVal)
    return (makeHandle newVal info newPeer)

sendWriterMsg :: MonadIO m => WriterToPeer -> PeerSpecific -> m ()
sendWriterMsg msg specific =
    let q = peerFromWriter specific in atomically $ writeTBQueue q msg

{- | This can be used to send a writer message to a specific peer

This does nothing if the peer isn't present
-}
sendWriterToPeer :: (MonadIO m, HasPeerInfo m) => WriterToPeer -> Peer -> m ()
sendWriterToPeer msg peer = do
    info      <- getPeerInfo
    maybeInfo <- HM.lookup peer <$> readTVarIO (infoMap info)
    whenJust maybeInfo (sendWriterMsg msg)

-- | Send a writer msg to every peer
sendWriterToAll :: (MonadIO m, HasPeerInfo m) => WriterToPeer -> m ()
sendWriterToAll msg = do
    info      <- getPeerInfo
    peerInfos <- HM.elems <$> readTVarIO (infoMap info)
    forM_ peerInfos (sendWriterMsg msg)

-- | Receive a message from a peer to a writer
recvToWriter :: (MonadIO m, HasPeerInfo m) => m PeerToWriter
recvToWriter = do
    info <- getPeerInfo
    atomically $ readTBQueue (infoToWriter info)
