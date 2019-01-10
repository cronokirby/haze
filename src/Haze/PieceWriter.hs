{-# LANGUAGE RecordWildCards #-}
{- |
Description: Contains functions centered around writing pieces to disk

This module contains functionality related to the concurrent
process responsible for periodically writing the pieces contained
in a shared buffer to disk. Utility functions for doing
the writing, as well as starting up the process are provided.
-}
module Haze.PieceWriter
    ( -- Mainly exported for testing
      makePieceInfo
    , writePieces
    )
where

import           Relude

import           Data.Array                     ( Array
                                                , (!)
                                                , elems
                                                , listArray
                                                )
import qualified Data.ByteString.Lazy          as LBS
import           Data.Maybe                     ( fromJust )
import           Path                           ( Path
                                                , Abs
                                                , File
                                                , Dir
                                                , (</>)
                                                )
import qualified Path
import qualified Path.IO                       as Path
import           System.IO                      ( Handle
                                                , IOMode(..)
                                                )

import           Haze.Tracker                   ( FileInfo(..)
                                                , FileItem(..)
                                                , SHAPieces(..)
                                                )


{- | Represents information about the pieces we'll be writing.

This should ideally be generated statically before running the piece writer,
as the information never changes.
-}
data PieceInfo = PieceInfo
    { pieceInfoRoot :: !(Path Abs Dir) -- ^ The root directory to work in
    -- | The structure of the pieces we're working with
    , pieceInfoStructure :: !PieceStructure
    }

-- | Represents information about the structure of pieces we have.
data PieceStructure
    -- | We have a single file, and an array of pieces to save
    = SimplePieces !(Path Abs File) !(Array Int (Path Abs File))


{- | Construct a 'PieceInfo' given information about the pieces.

The 'FileInfo' provides information about how the pieces are organised
in a file, and the 'SHAPieces' gives us information about how
each piece is sized. This function also takes a root directory
into which the files should be unpacked.
-}
makePieceInfo :: FileInfo -> SHAPieces -> Path Abs Dir -> PieceInfo
makePieceInfo fileInfo pieces root = case fileInfo of
    SingleFile (FileItem path fileLength _) ->
        let pieceSize  = shaPieceSize pieces
            maxPiece   = fromIntegral $ (fileLength - 1) `div` pieceSize
            paths      = makePiecePath root <$> [0 .. maxPiece]
            piecePaths = listArray (0, maxPiece) paths
        in  PieceInfo root (SimplePieces (root </> path) piecePaths)
    -- TODO: define this
    MultiFile _ _ -> undefined
  where
    makePiecePath :: Path Abs Dir -> Int -> Path Abs File
    makePiecePath theRoot piece =
        let pieceName = "piece-" ++ show piece ++ ".bin"
        in  theRoot </> fromJust (Path.parseRelFile pieceName)
    shaPieceSize :: SHAPieces -> Int64
    shaPieceSize (SHAPieces pieceSize _) = pieceSize


{- | Write a list of complete indices and pieces to a file.

This function takes information about the pieces, telling
it how they're arranged into files, as well as the size of each normal piece.
The function takes an absolute directory to serve as the root for all files.
-}
writePieces :: MonadIO m => PieceInfo -> [(Int, ByteString)] -> m ()
writePieces PieceInfo {..} pieces = case pieceInfoStructure of
    SimplePieces filePath piecePaths -> do
        forM_ pieces $ \(piece, bytes) ->
            writeFileBS (Path.fromAbsFile (piecePaths ! piece)) bytes
        let paths = elems piecePaths
        allPieces <- allM Path.doesFileExist paths
        when allPieces $ withAbsFile filePath AppendMode (appendAll paths)


-- | Utility function for `withFile` but with an absolute path
withAbsFile :: MonadIO m => Path Abs File -> IOMode -> (Handle -> IO ()) -> m ()
withAbsFile path mode action =
    liftIO $ withFile (Path.fromAbsFile path) mode action

-- | Append all paths in a file to a handle
appendAll :: [Path Abs File] -> Handle -> IO ()
appendAll paths = forM_ paths . appendH
  where
    moveBytes h = LBS.hGetContents >=> LBS.hPut h
    appendH h path = withAbsFile path ReadMode (moveBytes h)
