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
    ()
where

import           Relude

import           Control.Concurrent.STM.TBQueue ( TBQueue
                                                , newTBQueueIO
                                                )
import           Data.Array                     ( Array )
import qualified Data.HashMap.Strict           as HM

import           Haze.Messaging                 ( PeerToWriter(..)
                                                , WriterToPeer(..)
                                                , ManagerToPeer(..)
                                                )
import           Haze.PieceBuffer               ( PieceBuffer )
import           Haze.Tracker                   ( Peer )


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
    -- | The download rate for this peer
    , handleDLRate :: !(TVar Double)
    -- | The peer associated with this handle
    , handlePeer :: !Peer
    }

{- | PeerSpecific holds the information owned by one peer only.

As opposed to general structures like the piece rarity map,
each peer has its own communication channels, as well as other things.
To handle this, we have this struct for specific information
-}
data PeerSpecific = PeerSpecific
    { peerToWriter :: !(TBQueue PeerToWriter) -- ^ msgs to writer
    -- | A queue to allow the writer to send us messages
    , peerFromWriter :: !(TBQueue WriterToPeer)
    -- | A queue to allow the manager to send us messages
    , peerFromManager :: !(TBQueue ManagerToPeer)
    -- | The download rate for this specific peer
    , peerDLRate :: !(TVar Double)
    }

-- | Create a new empty struct of PeerSpecific Data
makePeerSpecific :: MonadIO m => m PeerSpecific
makePeerSpecific =
    PeerSpecific <$> mkQueue <*> mkQueue <*> mkQueue <*> newTVarIO 0
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
    -- | A map from a Peer to specific Peer data
    , infoMap :: !(TVar (HM.HashMap Peer PeerSpecific))
    }

-- | Make a handle from specific and shared information
makeHandle :: PeerSpecific -> PeerInfo -> Peer -> PeerHandle
makeHandle PeerSpecific {..} PeerInfo {..} = PeerHandle infoPieces
                                                        infoOurPieces
                                                        infoBuffer
                                                        peerToWriter
                                                        peerFromWriter
                                                        peerFromManager
                                                        peerDLRate

-- | Add a new peer to the information we have
addPeer :: MonadIO m => Peer -> PeerInfo -> m PeerHandle
addPeer newPeer info = do
    let mapVar = infoMap info
    newVal <- makePeerSpecific
    atomically $ modifyTVar' mapVar (HM.insert newPeer newVal)
    return (makeHandle newVal info newPeer)
