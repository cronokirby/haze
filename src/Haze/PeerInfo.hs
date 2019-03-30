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

import           Control.Concurrent.STM.TBQueue ( TBQueue )
import           Control.Concurrent.STM.TChan   ( TChan )
import           Data.Array                     ( Array )

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
    -- | The broadcast channel from the manager
    , handleFromManager :: !(TChan ManagerToPeer)
    -- | The donwload rate for this peer
    , handleDLRate :: !(TVar Double)
    -- | The peer associated with this handle
    , handlePeer :: !Peer
    }


data PeerInfo = PeerInfo
    { infoPieces :: !(Array Int (TVar Int)) -- ^ piece index -> count
    -- | The pieces we currently have
    , infoOurPieces :: !(TVar (Set Int))
    -- | The shared piece buffer
    , infoBuffer :: !(TVar PieceBuffer)
    -- | A broadcast channel for the manager to sned information to all peers
    , infoFromManager :: !(TChan ManagerToPeer)
    }