{- |
Description: Contains message types for communication between different components.

Messaging types are put here, to avoid circular dependency,
and to let us have an abstract specification of the adequate behavior.
-}
module Haze.Messaging 
    ( PeerToWriter(..)
    , SelectorToPeer(..)
    , PeerToSelector(..)
    , WriterToPeer(..)
    )
where

import Relude

import Haze.PieceBuffer (BlockIndex, BlockInfo)
import Haze.Tracker (Peer)


-- | Messages sent from peer clients to the piece writer
data PeerToWriter
    {- | The peer is requesting the data from a piece

    We send our own information in order to get a response
    -}
    = PieceRequest !Peer !BlockInfo
    -- | We've written to the piece buffer, and the writer should save 
    | PieceBufferWritten

-- | Messages sent from a writer to the peer
data WriterToPeer
    -- | The writer is fulfilling a 'PieceRequest'
    = PieceFulfilled !BlockIndex !ByteString
    -- | A new piece has just been saved
    | PieceAcquired !Int

-- | Messages sent from the selector to a peer
data SelectorToPeer
    -- | The peer should unchoke
    = PeerUnchoke
    -- | The peer should choke
    | PeerChoke
    -- | The peer should watch for interest, and then notify
    | PeerWatchForInterest

{- | Messages sent from a peer to the selector

The main purpose of these messages is to tell the selector
that a watched peer has become interested in downloading
from us.
-}
newtype PeerToSelector = PeerBecameInterested Peer