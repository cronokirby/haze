{-# LANGUAGE RecordWildCards #-}
{- |
Description: Contains functions for working with the file Buffer.

As we collect more and more Pieces of the file(s) we want to download
from peers, we need a data structure around which to choose pieces,
and to be able to fill with pieces. The data structure should also
let us save to a file.
-}
module Haze.PieceBuffer
    ( BlockIndex(..)
    , BlockInfo(..)
    , makeBlockInfo
    , PieceBuffer
    , makePieceBuffer
    , sizedPieceBuffer
    , nextBlock
    )
where

import           Relude

import           Data.Array                     ( Array
                                                , (!)
                                                , (//)
                                                , assocs
                                                , bounds
                                                , listArray
                                                )
import           Data.Ix                        ( Ix
                                                , inRange
                                                )

import           Haze.Tracker                   ( SHAPieces(..)
                                                , MetaInfo(..)
                                                , totalFileLength
                                                )


safeGet :: Ix i => Array i a -> i -> Maybe a
safeGet arr i | inRange (bounds arr) i = Just (arr ! i)
              | otherwise              = Nothing

putArr :: Ix i => i -> a -> Array i a -> Array i a
putArr ix val arr = arr // [(ix, val)]


-- | The size of a piece composing the torrent
type PieceSize = Int64

{- | The size of a block composing a piece

Blocks are 32 bit ints in size as specified by the p2p
protocol
-}
type BlockSize = Int


-- | Represents a buffer of pieces composing the file(s) to download
data PieceBuffer = PieceBuffer !SHAPieces !BlockSize !(Array Int Piece) deriving (Show)

-- | Represents one of the pieces composing 
data Piece
    -- | A fully downloaded, and saved piece
    = Saved
    -- | A complete, but not yet saved or checked piece 
    | Complete !ByteString
    -- | An incomplete set of blocks composing a this piece
    | Incomplete !(Array Int Block)
    deriving (Show)


{- | Represents a block of data sub dividing a piece

Blocks are the unit of data actually downloaded from peers,
and thus are the unit of data a peer can stake a claim on.
-}
data Block
    -- | An empty block no one has tagged
    = FreeBlock
    -- | A block that someone is downloading
    | TaggedBlock
    -- | A fully downloaded block
    | FullBlock !ByteString
    deriving (Eq, Show)


{- | Construct a piece buffer from total size, piece size, and block size

This exists mainly as a tool for testing the implementation of piece buffers.
usually you want to make a piece buffer corresponding to the configuration
of an actual torrent file, in which case 'makePieceBuffer' should be used
-}
sizedPieceBuffer :: Int64 -> SHAPieces -> BlockSize -> PieceBuffer
sizedPieceBuffer totalSize shaPieces@(SHAPieces pieceSize _) blockSize =
    let pieces        = makePiece blockSize <$> chunkSizes totalSize pieceSize
        maxPieceIndex = fromIntegral (div totalSize pieceSize) - 1
        pieceArr      = listArray (0, maxPieceIndex) pieces
    in  PieceBuffer shaPieces blockSize pieceArr
  where
    chunkSizes :: Integral a => a -> a -> [a]
    chunkSizes total size =
        let (d, m) = divMod total size 
            append = case m of
                0 -> []
                p -> [p]
        in replicate (fromIntegral d) d ++ append

{- | Construct a piece buffer given a block size and a torrent file

The block size controls the size of each downloadable chunk inside
of an individual piece composing the data to download. Usually
the default in this module should use.
-}
makePieceBuffer :: BlockSize -> MetaInfo -> PieceBuffer
makePieceBuffer blockSize MetaInfo {..} =
    let totalLength = totalFileLength metaFile
    in  sizedPieceBuffer totalLength metaPieces blockSize

{- Construct a new piece given the piece size, and the block size

Each piece in a torrent has the same size, except for the last one.
The block size can be set when constructing a piece buffer
-}
makePiece :: BlockSize -> PieceSize -> Piece
makePiece blockSize pieceSize =
    let maxBlockIndex = fromIntegral (div pieceSize (fromIntegral blockSize)) - 1
    in  Incomplete . listArray (0, maxBlockIndex) $ repeat FreeBlock


-- | Represents the index locating a block in the buffer
data BlockIndex = BlockIndex
    { blockPiece :: !Int -- ^ The index of the piece this is in
    , blockOffset :: !Int -- ^ The offset of this block inside the piece
    }
    deriving (Eq, Show)

-- | Represents the information about a block peers send to eachother
data BlockInfo = BlockInfo
    { blockIndex :: !BlockIndex -- ^ The location of this block
    , blockSize :: !BlockSize -- ^ How big this block is
    }
    deriving (Eq, Show)

{- | Construct a block info from the 3 pieces of information composing it

This is useful when parsing the structure off the wire.
-}
makeBlockInfo :: Int -> Int -> BlockSize -> BlockInfo
makeBlockInfo piece offset = BlockInfo (BlockIndex piece offset)


{- | Acquire and tag the next block in a piecebuffer

Returns Nothing if no block is free in that piece, or that piece
doesn't exist.
-}
nextBlock :: Int -> PieceBuffer -> (Maybe BlockInfo, PieceBuffer)
nextBlock piece buf@(PieceBuffer sha blockSize pieces) =
    maybe (Nothing, buf) (\(a, s) -> (Just a, s)) $ do
        blocks   <- getIncompletePiece pieces piece
        blockIdx <- findFreeBlock blocks
        let blocks' = putArr blockIdx TaggedBlock blocks
            pieces' = putArr piece (Incomplete blocks') pieces
            blockInfo = makeBlockInfo piece (blockIdx * blockSize) blockSize
        return (blockInfo, PieceBuffer sha blockSize pieces')
  where
    getIncompletePiece :: Array Int Piece -> Int -> Maybe (Array Int Block)
    getIncompletePiece arr ix = case safeGet arr ix of
        Just (Incomplete blocks) -> Just blocks
        _                        -> Nothing
    findFreeBlock :: Array Int Block -> Maybe Int
    findFreeBlock = fmap fst . find ((== FreeBlock) . snd) . assocs
