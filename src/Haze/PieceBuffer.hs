{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{- |
Description: Contains functions for working with the file Buffer.

As we collect more and more Pieces of the file(s) we want to download
from peers, we need a data structure around which to choose pieces,
and to be able to fill with pieces. The data structure should also
let us save to a file.

Note that all operations that take a specific piece index assume
that the piece is between the right bounds.
-}
module Haze.PieceBuffer
    ( BlockIndex(..)
    , BlockInfo(..)
    , makeBlockInfo
    , blockInfoMatches
    , PieceBuffer
    , makePieceBuffer
    , bufferArr
    , takeBlocks
    , writeBlock
    , saveCompletePieces
    , HasPieceBuffer(..)
    , takeBlocksM
    , writeBlockM
    , resetPieceM
    , saveCompletePiecesM
    )
where

import           Relude

import           Data.Array                     ( Array
                                                , assocs
                                                , bounds
                                                , elems
                                                , listArray
                                                )
import           Data.Array.Base                ( unsafeAt
                                                , unsafeArray
                                                , unsafeReplace
                                                )
import qualified Data.ByteString.Char8         as BS

import           Haze.Tracker                   ( SHAPieces(..)
                                                , MetaInfo(..)
                                                , totalFileLength
                                                )


unsafePutArr :: Int -> a -> Array Int a -> Array Int a
unsafePutArr ix val arr = unsafeReplace arr [(ix, val)]


-- | The size of a piece composing the torrent
type PieceSize = Int64

{- | The size of a block composing a piece

Blocks are 32 bit ints in size as specified by the p2p
protocol
-}
type BlockSize = Int


{- | Represents a buffer of pieces composing the file(s) to download

We keep track of the normal and last piece size in order to easily
reset a piece in case of a bad hash
-}
data PieceBuffer = PieceBuffer
    { pbNormalSize :: !PieceSize
    , pbLastSize :: !PieceSize
    , pbBlockSize :: !BlockSize
    , pbPieces :: !(Array Int (TVar Piece))
    }
    deriving (Eq)

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
    {- | A block tagged a certain number of times

    Initiailly all tagged blocks are 0, and then we try
    and tag a 0 block with a 1, then 1s with a 2 etc.
    -}
    = TaggedBlock Int
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

This acts as a no-op if the blocks if already full.
-}
fillTruncated :: ByteString -> BlockSize -> SizedBlock -> SizedBlock
fillTruncated bytes blockSize sized = case sized of
    NormalSized b       -> NormalSized (write blockSize b)
    SpecialSized size b -> SpecialSized size (write size b)
  where
    write size b = case b of
        f@(FullBlock _) -> f
        _               -> FullBlock (BS.take size bytes)


{- | Construct a piece buffer from total size, piece size, and block size

This exists mainly as a tool for testing the implementation of piece buffers.
usually you want to make a piece buffer corresponding to the configuration
of an actual torrent file, in which case 'makePieceBuffer' should be used
-}
sizedPieceBuffer
    :: MonadIO m => Int64 -> SHAPieces -> BlockSize -> m PieceBuffer
sizedPieceBuffer totalSize (SHAPieces pbNormalSize _) pbBlockSize = do
    let pbLastSize = case totalSize `mod` pbNormalSize of
            0 -> pbNormalSize
            x -> x
        chunks = chunkSizes totalSize pbNormalSize
        pieces = makePiece pbBlockSize <$> chunks
    pieceVars <- mapM newTVarIO pieces
    let pbPieces = unsafeArray (0, length chunks - 1) (zip [0 ..] pieceVars)
    return PieceBuffer { .. }
  where
    chunkSizes :: Integral a => a -> a -> [a]
    chunkSizes total size =
        let (d, m) = divMod total size
            append = if m == 0 then [] else [m]
        in  replicate (fromIntegral d) size ++ append

{- | Given a piece buffer, get a map with the same pieces

This is useful to reuse the work done in the piece buffer to
make a map from each piece.
-}
bufferArr :: PieceBuffer -> Array Int ()
bufferArr (PieceBuffer _ _ _ arr) = arr $> ()

{- | Construct a piece buffer given a block size and a torrent file

The block size controls the size of each downloadable chunk inside
of an individual piece composing the data to download. Usually
the default in this module should be used.
-}
makePieceBuffer :: MonadIO m => BlockSize -> MetaInfo -> m PieceBuffer
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
        append     = if m == 0 then [] else [SpecialSized m (TaggedBlock 0)]
        blocks     = replicate d (NormalSized (TaggedBlock 0)) ++ append
    in  Incomplete $ listArray (0, length blocks - 1) blocks


-- | Represents the index locating a block in the buffer
data BlockIndex = BlockIndex
    { blockPiece :: !Int -- ^ The index of the piece this is in
    , blockOffset :: !Int -- ^ The offset of this block inside the piece
    }
    deriving (Eq, Ord, Show)

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


{- | Acquire and tag N blocks in the piece buffer

Returns Nothing if that piece is completed.
Note that a completed piece might not have been saved yet.

The piece must have been verified to be in the right bounds before
calling this function.
-}
takeBlocks :: Int -> Int -> PieceBuffer -> STM (Maybe [BlockInfo])
takeBlocks amount pieceIdx PieceBuffer {..} = do
    let var = unsafeAt pbPieces pieceIdx
    piece <- readTVar var
    case piece of
        Saved             -> return Nothing
        Complete   _      -> return Nothing
        Incomplete blocks -> do
            let nextBlocks = findNextBlocks blocks
                blockInfos = map makeBlock nextBlocks
                blocks'    = unsafeReplace blocks nextBlocks
            blocks' `seq` writeTVar var (Incomplete blocks')
            return (Just blockInfos)
  where
    makeBlock :: (Int, SizedBlock) -> BlockInfo
    makeBlock (blockIdx, block) =
        let thisSize = getSize pbBlockSize block
        in  makeBlockInfo pieceIdx (blockIdx * pbBlockSize) thisSize
    getTag :: SizedBlock -> Maybe Int
    getTag sb =
        let block = getSizedBlock sb
        in  case block of
                TaggedBlock t -> Just t
                FullBlock   _ -> Nothing
    incrementTag :: SizedBlock -> SizedBlock
    incrementTag = fmap $ \case
        TaggedBlock t   -> TaggedBlock (t + 1)
        f@(FullBlock _) -> f
    findNextBlocks :: Array Int SizedBlock -> [(Int, SizedBlock)]
    findNextBlocks blocks =
        blocks
            & assocs
            & mapMaybe (\(i, b) -> (,) (i, b) <$> getTag b)
            & sortBy (compare `on` snd)
            & map (fmap incrementTag . fst)
            & take amount

{- | Write the data associated with a block to the buffer.

Any data longer than the block size of the piece buffer will be discarded.
Clients should check that block that they received matches their expectations
before calling this function.

This acts as a NoOp if that block is already written.
In theory, no one should write to the same block twice anyways,
if 'nextBlock' is used correctly, since each block will be tagged,
so multiple processes won't jump on the same block.
-}
writeBlock :: BlockIndex -> ByteString -> PieceBuffer -> STM ()
writeBlock BlockIndex {..} bytes PieceBuffer {..} = do
    let var = unsafeAt pbPieces blockPiece
    piece <- readTVar var
    case piece of
        Saved             -> return ()
        Complete   _      -> return ()
        Incomplete blocks -> do
            let blockIdx = blockOffset `div` pbBlockSize
                block    = unsafeAt blocks blockIdx
                filled   = fillTruncated bytes pbBlockSize block
                blocks'  = unsafePutArr blockIdx filled blocks
            blocks' `seq` writeTVar var (completePiece blocks')
  where
    completePiece :: Array Int SizedBlock -> Piece
    completePiece blocks =
        let allBytes = traverse getFullBlock (elems blocks)
        in  maybe (Incomplete blocks) (Complete . mconcat) allBytes

{- | Reset a piece back to its initial state.

This should be used in case a piece hashes to wrong
thing, for whatever reason. In that case, we want
to redownload the piece entirely, and thus, get
rid of the data currently in the buffer.
-}
resetPiece :: Int -> PieceBuffer -> STM ()
resetPiece pieceIdx PieceBuffer {..} = do
    let lastPiece = snd (bounds pbPieces)
        pieceSize = if pieceIdx /= lastPiece then pbNormalSize else pbLastSize
        newPiece  = makePiece pbBlockSize pieceSize
        pieceVar  = unsafeAt pbPieces pieceIdx
    newPiece `seq` writeTVar pieceVar newPiece

{- | Take out all complete pieces from the buffer.

The main utility in this function is in saving the piece buffer to disk.
This function returns a list of (pieceIndex, bytes) ready to save,
and removes these pieces from the buffer itself.
-}
saveCompletePieces :: PieceBuffer -> STM [(Int, ByteString)]
saveCompletePieces PieceBuffer {..} = do
    pairs <- forM (assocs pbPieces) $ \(i, var) -> do
        piece <- readTVar var
        case piece of
            Complete bytes -> do
                writeTVar var Saved
                return (Just (i, bytes))
            _ -> return Nothing
    return (catMaybes pairs)


-- | Represents a class of contexts in which we have access to a piecebuffer
class HasPieceBuffer m where
    getPieceBuffer :: m PieceBuffer

-- | Get and tag a certain number of blocks from the buffer
takeBlocksM
    :: (MonadIO m, HasPieceBuffer m) => Int -> Int -> m (Maybe [BlockInfo])
takeBlocksM amount piece =
    getPieceBuffer >>= atomically . takeBlocks amount piece

-- | Commit a block to a shared buffer
writeBlockM :: (MonadIO m, HasPieceBuffer m) => BlockIndex -> ByteString -> m ()
writeBlockM index bytes =
    getPieceBuffer >>= atomically . writeBlock index bytes

-- | Reset a specific piece to its initial state
resetPieceM :: (MonadIO m, HasPieceBuffer m) => Int -> m ()
resetPieceM piece = getPieceBuffer >>= atomically . resetPiece piece

-- | Save the completed pieces to the shared buffer
saveCompletePiecesM :: (MonadIO m, HasPieceBuffer m) => m [(Int, ByteString)]
saveCompletePiecesM = getPieceBuffer >>= atomically . saveCompletePieces
