{- |
Description: Contains functions for working with the file Buffer.

As we collect more and more Pieces of the file(s) we want to download
from peers, we need a data structure around which to choose pieces,
and to be able to fill with pieces. The data structure should also
let us save to a file.
-}
module Haze.PieceBuffer
    (
    )
where

import Relude

import Data.Array (Array)


-- | Represents a buffer of pieces composing the file(s) to download
newtype PieceBuffer = PieceBuffer (Array Int Piece)


-- | Represents one of the pieces composing 
data Piece
    -- | A fully downloaded, and saved piece
    = Saved
    -- | A complete, but not yet saved or checked piece 
    | Complete !ByteString
    -- | An incomplete set of blocks composing a this piece
    | Incomplete !(Array Int Block)



-- | Represents a block of data sub dividing a piece
-- Blocks are the unit of data actually downloaded from peers,
-- and thus are the unit of data a peer can stake a claim on.
data Block
    -- | An empty block no one has tagged
    = FreeBlock
    -- | A block that someone is downloading
    | TaggedBlock
    -- | A fully downloaded block
    | FullBlock !ByteString
