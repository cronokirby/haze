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
    , sizedPieceBuffer
    , bufferArr
    , takeBlocks
    , writeBlock
    , saveCompletePieces
    , bufferBytes
    , HasPieceBuffer(..)
    , takeBlocksM
    , writeBlockM
    , resetPieceM
    , saveCompletePiecesM
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


{- | Represents a buffer of pieces composing the file(s) to download

We keep track of the normal and last piece size in order to easily
reset a piece in case of a bad hash
-}
data PieceBuffer = PieceBuffer !PieceSize !PieceSize !BlockSize !(Array Int Piece)
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
sizedPieceBuffer totalSize (SHAPieces pieceSize _) blockSize =
    let chunks   = chunkSizes totalSize pieceSize
        pieces   = makePiece blockSize <$> chunks
        pieceArr = listArray (0, length chunks - 1) pieces
    in  PieceBuffer pieceSize (totalSize `mod` pieceSize) blockSize pieceArr
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
-}
takeBlocks :: Int -> Int -> PieceBuffer -> (Maybe [BlockInfo], PieceBuffer)
takeBlocks amount piece buf@(PieceBuffer n l blockSize pieces) =
    case getIncompletePiece pieces piece of
        Nothing -> (Nothing, buf)
        Just blocks ->
            let nextBlocks = findNextBlocks blocks
                blocks'    = blocks // nextBlocks
                pieces'    = putArr piece (Incomplete blocks') pieces
                blockInfos = map makeBlock nextBlocks
            in  (Just blockInfos, PieceBuffer n l blockSize pieces')
  where
    getIncompletePiece :: Array Int Piece -> Int -> Maybe (Array Int SizedBlock)
    getIncompletePiece arr ix = case safeGet arr ix of
        Just (Incomplete blocks) -> Just blocks
        _                        -> Nothing
    makeBlock :: (Int, SizedBlock) -> BlockInfo
    makeBlock (blockIdx, block) =
        let thisSize = getSize blockSize block
        in  makeBlockInfo piece (blockIdx * blockSize) thisSize
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
writeBlock :: BlockIndex -> ByteString -> PieceBuffer -> PieceBuffer
writeBlock BlockIndex {..} bytes buf@(PieceBuffer n l blockSize pieces) =
    fromMaybe buf $ do
        blocks <- incompleteBlocks
        let blockIdx = blockOffset `div` blockSize
        block  <- safeGet blocks blockIdx
        filled <- fillTruncated bytes blockSize block
        let blocks' = putArr blockIdx filled blocks
            pieces' = putArr blockPiece (completePiece blocks') pieces
        return (PieceBuffer n l blockSize pieces')
  where
    incompleteBlocks :: Maybe (Array Int SizedBlock)
    incompleteBlocks = case safeGet pieces blockPiece of
        Just (Incomplete blocks) -> Just blocks
        _                        -> Nothing
    completePiece :: Array Int SizedBlock -> Piece
    completePiece blocks =
        let allBytes = traverse getFullBlock (elems blocks)
        in  maybe (Incomplete blocks) (Complete . mconcat) allBytes

{- | Reset a piece back to its initial state.

This should be used in case a piece hases to wrong
thing, for whatever reason. In that case, we want
to redownload the piece entirely, and thus, get
rid of the data currently in the buffer.
-}
resetPiece :: Int -> PieceBuffer -> PieceBuffer
resetPiece piece (PieceBuffer normal bookEnd blockSize pieces) =
    let lastPiece = snd (bounds pieces)
        pieceSize = if piece /= lastPiece || bookEnd == 0 
            then normal
            else bookEnd
        newPiece = makePiece blockSize pieceSize
        pieces' = putArr piece newPiece pieces
    in PieceBuffer normal bookEnd blockSize pieces'


{- | Take out all complete pieces from the buffer.

The main utility in this function is in saving the piece buffer to disk.
This function returns a list of (pieceIndex, bytes) ready to save,
and removes these pieces from the buffer itself.
-}
saveCompletePieces :: PieceBuffer -> ([(Int, ByteString)], PieceBuffer)
saveCompletePieces (PieceBuffer n l size pieces) =
    let extractPiece (i, p) = (,) i <$> getComplete p
        complete = catMaybes $ extractPiece <$> assocs pieces
        pieces'  = pieces // map (\(i, _) -> (i, Saved)) complete
        buffer'  = PieceBuffer n l size pieces'
    in  (complete, buffer')
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
bufferBytes (PieceBuffer pieceSize _ _ pieces) = foldMap
    pieceBytes
    (elems pieces)
  where
    size = fromIntegral pieceSize
    pieceBytes :: Piece -> ByteString
    pieceBytes Saved           = BS.replicate size 's'
    pieceBytes (Complete   bs) = bs
    pieceBytes (Incomplete _ ) = BS.replicate size '_'



-- Utility function to apply a stateful function to a TVar
stateTVar :: MonadIO m => (s -> (a, s)) -> TVar s -> m a
stateTVar f var = atomically $ do
    s <- readTVar var
    let (a, s') = f s
    writeTVar var $! s'
    return a


-- | Represents a class of contexts in which we have access to a piecebuffer
class HasPieceBuffer m where
    getPieceBuffer :: m (TVar PieceBuffer)

-- | Get and tag a certain number of blocks from the buffer
takeBlocksM
    :: (MonadIO m, HasPieceBuffer m) => Int -> Int -> m (Maybe [BlockInfo])
takeBlocksM amount piece =
    getPieceBuffer >>= stateTVar (takeBlocks amount piece)

-- | Commit a block to a shared buffer
writeBlockM :: (MonadIO m, HasPieceBuffer m) => BlockIndex -> ByteString -> m ()
writeBlockM index bytes = do
    var <- getPieceBuffer
    atomically $ modifyTVar' var (writeBlock index bytes)

-- | Reset a specific piece to its initial state
resetPieceM :: (MonadIO m, HasPieceBuffer m) => Int -> m ()
resetPieceM piece = do
    var <- getPieceBuffer
    atomically $ modifyTVar' var (resetPiece piece)

-- | Save the completed pieces to the shared buffer
saveCompletePiecesM :: (MonadIO m, HasPieceBuffer m) => m [(Int, ByteString)]
saveCompletePiecesM = getPieceBuffer >>= stateTVar saveCompletePieces
