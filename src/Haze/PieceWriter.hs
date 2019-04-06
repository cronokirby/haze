{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Description: Contains functions centered around writing pieces to disk

This module contains functionality related to the concurrent
process responsible for periodically writing the pieces contained
in a shared buffer to disk. Utility functions for doing
the writing, as well as starting up the process are provided.
-}
module Haze.PieceWriter
    ( -- Mainly exported for testing
      FileStructure(..)
    , SplitPiece(..)
    , makeFileStructure
    , writePieces
    , PieceMapping(..)
    , PieceLocation(..)
    , CompleteLocation(..)
    , EmbeddedLocation(..)
    , makeMapping
    )
where

import           Relude

import           Data.Array                     ( Array
                                                , (!)
                                                , elems
                                                , listArray
                                                )
import qualified Data.ByteString               as BS
-- We import lazy bytestring for implementing efficient file ops
import qualified Data.ByteString.Lazy          as LBS
import           Data.List                      ( nub )
import           Data.Maybe                     ( fromJust )
import           Path                           ( Path
                                                , Abs
                                                , File
                                                , Dir
                                                , (</>)
                                                , (<.>)
                                                )
import qualified Path
import qualified Path.IO                       as Path
import           System.IO                      ( Handle
                                                , IOMode(..)
                                                , SeekMode(..)
                                                , hSeek
                                                )

import           Haze.Messaging                 ( PeerToWriter(..)
                                                , WriterToPeer(..)
                                                )
import           Haze.PeerInfo                  ( HasPeerInfo(..)
                                                , PeerInfo(..)
                                                , recvToWriter
                                                , sendWriterToPeer
                                                , sendWriterToAll
                                                )
import           Haze.PieceBuffer               ( BlockIndex(..)
                                                , BlockInfo(..)
                                                , HasPieceBuffer(..)
                                                , saveCompletePiecesM
                                                )
import           Haze.Tracker                   ( FileInfo(..)
                                                , FileItem(..)
                                                , SHAPieces(..)
                                                , totalFileLength
                                                )



type AbsFile = Path Abs File
type AbsDir = Path Abs Dir

makePiecePath :: AbsDir -> Int -> AbsFile
makePiecePath theRoot piece =
    let pieceName = "piece-" ++ show piece ++ ".bin"
    in  theRoot </> fromJust (Path.parseRelFile pieceName)

makeStartPiece :: AbsFile -> AbsFile
makeStartPiece file = fromJust (file <.> "start")

makeEndPiece :: AbsFile -> AbsFile
makeEndPiece file = fromJust (file <.> "end")


{- | Represents information about the structure of pieces we have.

This should ideally be generated statically before running the piece writer,
as this information never changes.
-}
data FileStructure = FileStructure !(Array Int SplitPiece) ![(AbsFile, [AbsFile])]
    deriving (Eq, Show)

-- | Represents a piece we have to save potentially over 2 files.
data SplitPiece
    -- | A piece we can save to a piece file
    = NormalPiece !AbsFile
    -- | A piece that needs to save variable numbers of bytes in multiple files
    | SplitPieces ![(Int, AbsFile)]
    deriving (Eq, Show)


{- | Construct a 'FileStructure' given information about the pieceMapping

Knowing how each piece can be retrieved from disk is enough to
figure out how to save them to disk.
-}
makeFileStructure :: PieceMapping -> FileStructure
makeFileStructure (PieceMapping m) =
    let locations = join (elems m)
        files     = nub $ map extractEmbedded locations
        deps      = map (\f -> (f, mapMaybe (getDep f) locations)) files
        splits = locationsToSplit <$> m
    in  FileStructure splits deps
  where
    extractEmbedded :: PieceLocation -> AbsFile
    extractEmbedded (PieceLocation (EmbeddedLocation e _ _) _) = e
    getDep :: AbsFile -> PieceLocation -> Maybe AbsFile
    getDep file (PieceLocation (EmbeddedLocation e _ _) (CompleteLocation f))
        | file == e = Just f
        | otherwise = Nothing
    locationsToSplit :: [PieceLocation] -> SplitPiece
    locationsToSplit [] = error "No locations to split"
    locationsToSplit [PieceLocation _ (CompleteLocation f)] = NormalPiece f
    locationsToSplit plenty =
        let
            getSplit (PieceLocation (EmbeddedLocation _ _ l) (CompleteLocation f))
                = (l, f)
        in  SplitPieces (map getSplit plenty)



{- | Write a list of complete indices and pieces to a file.

This function takes information about the pieces, telling
it how they're arranged into files, as well as the size of each normal piece.
The function takes an absolute directory to serve as the root for all files.
-}
writePieces :: MonadIO m => FileStructure -> [(Int, ByteString)] -> m ()
writePieces (FileStructure splitPieces deps) pieces = do
    forM_ pieces $ \(piece, bytes) -> case splitPieces ! piece of
        NormalPiece filePath -> writeFileBS (Path.fromAbsFile filePath) bytes
        SplitPieces splits ->
            let go (bs, action) (size, file) =
                    let (start, end) = BS.splitAt size bs
                    in  (end, action *> writeAbsFile file start)
            in  snd $ foldl' go (bytes, pure ()) splits
    forM_ deps (uncurry appendWhenAllExist)
  where
    -- This will also remove the appended files
    appendWhenAllExist :: MonadIO m => AbsFile -> [AbsFile] -> m ()
    appendWhenAllExist filePath paths = do
        allPieces <- allM Path.doesFileExist paths
        when allPieces $ do
            withAbsFile filePath AppendMode (appendAll paths)
            removeAll paths


-- | Write bytes to an absolute path
writeAbsFile :: MonadIO m => AbsFile -> ByteString -> m ()
writeAbsFile path = writeFileBS (Path.fromAbsFile path)

-- | Utility function for `withFile` but with an absolute path
withAbsFile :: MonadIO m => AbsFile -> IOMode -> (Handle -> IO a) -> m a
withAbsFile path mode action =
    liftIO $ withFile (Path.fromAbsFile path) mode action

-- | Append all paths in a file to a handle
appendAll :: [AbsFile] -> Handle -> IO ()
appendAll paths = forM_ paths . appendH
  where
    moveBytes h = LBS.hGetContents >=> LBS.hPut h
    appendH h path = withAbsFile path ReadMode (moveBytes h)

-- | Remove all files in a list
removeAll :: MonadIO m => [AbsFile] -> m ()
removeAll paths = forM_ paths Path.removeFile

{- | PieceMapping allows us to determine where to find a piece on disk.

It contains a list of locations that compose the piece. The locations
come in the same order as they fill the piece in.

This is similar to FileStructure, except instead of telling us how to save
a piece to disk, this tells us how to retrieve the disk later on.
Retrieving is made complicated by the fact that where a piece is located
changes as we download more of the file. In the case of a single file,
the piece will be stored in its own file until the entire file is downloaded,
at which point the piece occupies just a section of a big file.
With multiple files, we have the same situation, but with multiple files
each time. A further complication with multiple files is that the piece
may be a section of one file, but not yet integrated into a part of another file.
-}
newtype PieceMapping = PieceMapping (Array Int [PieceLocation])
    deriving (Eq, Show)

-- | Create a PieceMapping given the structure of the files
makeMapping :: FileInfo -> SHAPieces -> AbsDir -> PieceMapping
makeMapping fileInfo (SHAPieces pieceSize _) root =
    let (root', items) = case fileInfo of
            SingleFile item      -> (root, [item])
            MultiFile relRoot is -> (root </> relRoot, is)
        makeEmbedded = findEmbedded root' items
        makeLocation = fillLocation root'
        embeds       = zipWith makeEmbedded pieceOffsets pieceLengths
        locations    = zipWith makeLocation [0 ..] embeds
    in  PieceMapping (listArray (0, length locations - 1) locations)
  where
    totalSize :: Int64
    totalSize = totalFileLength fileInfo
    pieceOffsets :: [Int64]
    pieceOffsets = [0, pieceSize ..]
    pieceLengths :: [Int64]
    pieceLengths =
        let normalPieceCount = fromIntegral $ totalSize `div` pieceSize
            leftoverLength = totalSize `mod` pieceSize
            leftOver = if leftoverLength == 0 then [] else [leftoverLength]
        in  replicate normalPieceCount pieceSize ++ leftOver


-- | An integer offset into a file
type Offset = Int64

{- | PieceLocation represents a recipe to get part of a piece from disk.

It contains both the initial location, where the piece is alone,
and the final embedded location.
The piece is only in one of them at a time, but where it is needs to be
checked by actually looking if the file for the embedded location is written.
-}
data PieceLocation = PieceLocation !EmbeddedLocation !CompleteLocation
    deriving (Eq, Show)

-- | A place where the piece is stored in its own file
newtype CompleteLocation = CompleteLocation AbsFile
    deriving (Eq, Show)

-- | The piece is lodged inside a larger file
data EmbeddedLocation = EmbeddedLocation !AbsFile !Offset !Int
    deriving (Eq, Show)

findEmbedded :: AbsDir -> [FileItem] -> Offset -> Int64 -> [EmbeddedLocation]
findEmbedded _ [] _ _ = []
findEmbedded dir (FileItem path size _ : rest) offset ln
    | offset < size && offset + ln <= size
    = [EmbeddedLocation (dir </> path) offset (fromIntegral ln)]
    | offset < size
    = let endLength  = size - offset
          endLengthI = fromIntegral endLength
          embed      = EmbeddedLocation (dir </> path) offset endLengthI
      in  embed : findEmbedded dir rest 0 (ln - endLength)
    | otherwise
    = findEmbedded dir rest (offset - size) ln

fillLocation :: AbsDir -> Int -> [EmbeddedLocation] -> [PieceLocation]
fillLocation root piece embeds =
    let completes = makeCompletes embeds
    in  zipWith PieceLocation embeds completes
  where
    makeCompletes :: [EmbeddedLocation] -> [CompleteLocation]
    makeCompletes []  = []
    makeCompletes [_] = [CompleteLocation (makePiecePath root piece)]
    makeCompletes (EmbeddedLocation file _ _ : rest) =
        CompleteLocation (makeEndPiece file) : map makeStarts rest
    makeStarts :: EmbeddedLocation -> CompleteLocation
    makeStarts (EmbeddedLocation file _ _) =
        CompleteLocation (makeStartPiece file)


-- | using a pieceMapping, get the nth piece from disk
getPiece :: MonadIO m => PieceMapping -> Int -> m ByteString
getPiece (PieceMapping mappings) piece =
    let mapping = mappings ! piece in foldMapA getLocation mapping
  where
    getLocation :: MonadIO m => PieceLocation -> m ByteString
    getLocation (PieceLocation embedded (CompleteLocation cl)) = do
        isComplete <- Path.doesFileExist cl
        if isComplete then readComplete cl else readEmbedded embedded
    readComplete :: MonadIO m => AbsFile -> m ByteString
    readComplete = readFileBS . Path.fromAbsFile
    readEmbedded :: MonadIO m => EmbeddedLocation -> m ByteString
    readEmbedded (EmbeddedLocation file offset amount) =
        withAbsFile file ReadMode $ \h -> do
            hSeek h AbsoluteSeek (fromIntegral offset)
            BS.hGet h amount



-- | Represents the data a piece writer needs
data PieceWriterInfo = PieceWriterInfo
    { pieceStructure :: !FileStructure
    , pieceMapping :: !PieceMapping
    , peerInfo :: !PeerInfo
    }

-- | A context with access to what a piece writer process needs
newtype PieceWriterM a = PieceWriterM (ReaderT PieceWriterInfo IO a)
    deriving (Functor, Applicative, Monad, MonadReader PieceWriterInfo, MonadIO)

instance HasPeerInfo PieceWriterM where
    getPeerInfo = asks peerInfo

instance HasPieceBuffer PieceWriterM where
    getPieceBuffer = asks (infoBuffer . peerInfo)

-- | Run a piece writer function given the right context
runPieceWriterM :: PieceWriterInfo -> PieceWriterM a -> IO a
runPieceWriterM info (PieceWriterM reader) = runReaderT reader info

-- | Lookup and write the pieces in a pieceBuff
writePiecesM :: PieceWriterM ()
writePiecesM = do
    pieces <- saveCompletePiecesM
    info   <- asks pieceStructure
    writePieces info pieces

pieceWriterLoop :: PieceWriterM ()
pieceWriterLoop = forever $ do
    msg <- recvToWriter
    case msg of
        PieceBufferWritten     -> writePiecesM
        PieceRequest peer info -> do
            let (BlockInfo index@(BlockIndex piece offset) size) = info
            mapping   <- asks pieceMapping
            pieceData <- getPiece mapping piece
            let block = BS.take size $ BS.drop offset pieceData
            sendWriterToPeer (PieceFulfilled index block) peer

