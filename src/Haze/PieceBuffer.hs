{-# LANGUAGE DeriveFunctor   #-}
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
    , blockInfoMatches
    , PieceBuffer
    , makePieceBuffer
    , sizedPieceBuffer
    , nextBlock
    , writeBlock
    , saveCompletePieces
    , bufferBytes
    )
where

import           Relude

import           Data.Array                     ( Array
                                                , (!)
                                                , (//)
                                                , assocs
                                                , bounds
                                                , elems
                                                , listArray
                                                )
import qualified Data.ByteString.Char8         as BS
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
data PieceBuffer = PieceBuffer !SHAPieces !BlockSize !(Array Int Piece)
    deriving (Eq, Show)

-- | Represents one of the pieces composing 
data Piece
    -- | A fully downloaded, and saved piece
    = Saved
    -- | A complete, but not yet saved or checked piece 
    | Complete !ByteString
    -- | An incomplete set of blocks composing a this piece
    | Incomplete !(Array Int SizedBlock)
    deriving (Eq, Show)


{- | Represents something that may be normally sized, or specially sized

This type is generic mainly to be able to derive Functor, but is 
predominantly used with 'BlockSize' and 'Block'
-}
data Sized s a
    -- | This piece of data is implicitly sized
    = NormalSized a
    -- | This piece of data has a specific size
    | SpecialSized s a
    deriving (Eq, Functor, Show)

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

type SizedBlock = Sized BlockSize Block

-- | Extract out the block from a sized block
getSizedBlock :: Sized s Block -> Block
getSizedBlock (NormalSized b   ) = b
getSizedBlock (SpecialSized _ b) = b

-- | Get the size using a default for Normal things
getSize :: s -> Sized s a -> s
getSize normalSize sized = case sized of
    NormalSized _       -> normalSize
    SpecialSized size _ -> size

-- | Try and extract out the bytes composing a block
getFullBlock :: SizedBlock -> Maybe ByteString
getFullBlock = getFullBytes . getSizedBlock
  where
    getFullBytes (FullBlock bs) = Just bs
    getFullBytes _              = Nothing

{- | Fill a size block, truncating the ByteString if necessary

Returns 'Nothing' if the block was already full.
-}
fillTruncated :: ByteString -> BlockSize -> SizedBlock -> Maybe SizedBlock
fillTruncated bytes blockSize sized = case sized of
    NormalSized b       -> NormalSized <$> write blockSize b
    SpecialSized size b -> SpecialSized size <$> write size b
  where
    write size b = case b of
        FullBlock _ -> Nothing
        _           -> Just (FullBlock (BS.take size bytes))


{- | Construct a piece buffer from total size, piece size, and block size

This exists mainly as a tool for testing the implementation of piece buffers.
usually you want to make a piece buffer corresponding to the configuration
of an actual torrent file, in which case 'makePieceBuffer' should be used
-}
sizedPieceBuffer :: Int64 -> SHAPieces -> BlockSize -> PieceBuffer
sizedPieceBuffer totalSize shaPieces@(SHAPieces pieceSize _) blockSize =
    let chunks   = chunkSizes totalSize pieceSize
        pieces   = makePiece blockSize <$> chunks
        pieceArr = listArray (0, length chunks - 1) pieces
    in  PieceBuffer shaPieces blockSize pieceArr
  where
    chunkSizes :: Integral a => a -> a -> [a]
    chunkSizes total size =
        let (d, m) = divMod total size
            append = if m == 0 then [] else [m]
        in  replicate (fromIntegral d) size ++ append

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
    let (d64, m64) = divMod pieceSize (fromIntegral blockSize)
        -- Do the conversion after the division to avoid overflow
        d          = fromIntegral d64
        m          = fromIntegral m64
        append     = if m == 0 then [] else [SpecialSized m FreeBlock]
        blocks     = replicate d (NormalSized FreeBlock) ++ append
    in  Incomplete $ listArray (0, length blocks - 1) blocks


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

{- | Returns true if a received block matches its description

This is useful for peers to check that they received a block corresponding
to their expectations.
-}
blockInfoMatches :: BlockInfo -> BlockIndex -> ByteString -> Bool
blockInfoMatches BlockInfo {..} index bytes =
    index == blockIndex && BS.length bytes == blockSize


{- | Acquire and tag the next block in a piece

Returns Nothing if no block is free in that piece, or that piece
doesn't exist.
-}
nextBlock :: Int -> PieceBuffer -> (Maybe BlockInfo, PieceBuffer)
nextBlock piece buf@(PieceBuffer sha blockSize pieces) =
    maybe (Nothing, buf) (\(a, s) -> (Just a, s)) $ do
        blocks            <- getIncompletePiece pieces piece
        (blockIdx, block) <- findFreeBlock blocks
        let blocks'   = putArr blockIdx (block $> TaggedBlock) blocks
            pieces'   = putArr piece (Incomplete blocks') pieces
            thisSize  = getSize blockSize block
            blockInfo = makeBlockInfo piece (blockIdx * blockSize) thisSize
        return (blockInfo, PieceBuffer sha blockSize pieces')
  where
    getIncompletePiece :: Array Int Piece -> Int -> Maybe (Array Int SizedBlock)
    getIncompletePiece arr ix = case safeGet arr ix of
        Just (Incomplete blocks) -> Just blocks
        _                        -> Nothing
    findFreeBlock :: Array Int SizedBlock -> Maybe (Int, SizedBlock)
    findFreeBlock = find ((== FreeBlock) . getSizedBlock . snd) . assocs

{- | Write the data associated with a block to the buffer.

Any data longer than the block size of the piece buffer will be discarded.
Clients should check that block that they received matches their expectations
before calling this function.

This acts as a NoOp if that block is already written.
In theory, no one should write to the same block twice anyways,
if 'nextBlock' is used correctly, since each block will be tagged,
so multiple processes won't jump on the same block.
-}
writeBlock :: BlockIndex -> ByteString -> PieceBuffer -> PieceBuffer
writeBlock BlockIndex {..} bytes buf@(PieceBuffer sha blockSize pieces) =
    fromMaybe buf $ do
        blocks <- incompleteBlocks
        let blockIdx = blockOffset `div` blockSize
        block  <- safeGet blocks blockIdx
        filled <- fillTruncated bytes blockSize block
        let blocks' = putArr blockIdx filled blocks
            pieces' = putArr blockPiece (completePiece blocks') pieces
        return (PieceBuffer sha blockSize pieces')
  where
    incompleteBlocks :: Maybe (Array Int SizedBlock)
    incompleteBlocks = case safeGet pieces blockPiece of
        Just (Incomplete blocks) -> Just blocks
        _                        -> Nothing
    completePiece :: Array Int SizedBlock -> Piece
    completePiece blocks =
        let allBytes = traverse getFullBlock (elems blocks)
        in  maybe (Incomplete blocks) (Complete . mconcat) allBytes


{- | Take out all complete pieces from the buffer.

The main utility in this function is in saving the piece buffer to disk.
This function returns a list of (pieceIndex, bytes) ready to save,
and removes these pieces from the buffer itself.
-}
saveCompletePieces :: PieceBuffer -> ([(Int, ByteString)], PieceBuffer)
saveCompletePieces (PieceBuffer sha size pieces) =
    let extractPiece (i, p) = (,) i <$> getComplete p
        complete = catMaybes $ extractPiece <$> assocs pieces
        pieces' = pieces // map (\(i, _) -> (i, Saved)) complete
        buffer' = PieceBuffer sha size pieces'
    in (complete, buffer')
  where
    getComplete :: Piece -> Maybe ByteString
    getComplete (Complete bytes) = Just bytes
    getComplete _                = Nothing


{- | Represent the piece buffer as a full bytestring.

This could be very large! This is useful mainly for testing and
debugging. This function also ignores file boundaries.

This only displays complete pieces, and not the blocks inside of them.

Since this is used for debugging, empty portions are represented as
"_", and saved portions as "s".
-}
bufferBytes :: PieceBuffer -> ByteString
bufferBytes (PieceBuffer (SHAPieces pieceSize _) _ pieces) = foldMap
    pieceBytes
    (elems pieces)
  where
    size = fromIntegral pieceSize
    pieceBytes :: Piece -> ByteString
    pieceBytes Saved           = BS.replicate size 's'
    pieceBytes (Complete   bs) = bs
    pieceBytes (Incomplete _ ) = BS.replicate size '_'
