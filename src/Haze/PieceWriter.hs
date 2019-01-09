{- |
Description: Contains functions centered around writing pieces to disk

This module contains functionality related to the concurrent
process responsible for periodically writing the pieces contained
in a shared buffer to disk. Utility functions for doing
the writing, as well as starting up the process are provided.
-}
module Haze.PieceWriter
    ( writePieces -- exported mainly for testing
    )
where

import           Relude

import           Data.Array                     ( (!)
                                                , listArray
                                                )
import qualified Data.ByteString.Lazy          as LBS
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


{- | Represents information about the pieces we'll be writing

This information is usually static, and provided to us before
we start the piece writer.
-}
data PieceInfo = PieceInfo
    { pieceInfoFile :: FileInfo -- ^ The file info for these pieces
    , pieceInfoSHA :: SHAPieces -- ^ The piece size and sha for these pieces
    }

-- | Get the size of an individual piece from the information.
pieceInfoSize :: PieceInfo -> Int64
pieceInfoSize (PieceInfo _ (SHAPieces size _)) = size


{- | Write a list of complete indices and pieces to a file.

This function takes information about the pieces, telling
it how they're arranged into files, as well as the size of each normal piece.
The function takes an absolute directory to serve as the root for all files.
-}
writePieces
    :: MonadIO m => PieceInfo -> Path Abs Dir -> [(Int, ByteString)] -> m ()
writePieces pieceInfo root pieces = case pieceInfoFile pieceInfo of
    SingleFile item -> do
        let (FileItem path fileLength _) = item
            pieceSize = pieceInfoSize pieceInfo
            maxPiece = fromIntegral $ (fileLength - 1) `div` pieceSize
        -- this shouldn't ever throw
        paths <- forM [0 .. maxPiece] (makePiecePath root)
        let piecePaths = listArray (0, maxPiece) paths
        forM_ pieces $ \(piece, bytes) ->
            writeFileBS (Path.fromAbsFile (piecePaths ! piece)) bytes
        allPieces <- allM Path.doesFileExist paths
        when allPieces
            $ withAbsFile (root </> path) AppendMode (appendAll paths)
    -- TODO: define this
    MultiFile _ _ -> undefined
  where
    makePiecePath :: MonadIO m => Path Abs Dir -> Int -> m (Path Abs File)
    makePiecePath theRoot piece =
        let pieceName = "piece-" ++ show piece ++ ".bin"
        in  (theRoot </>) <$> liftIO (Path.parseRelFile pieceName)
    appendH :: MonadIO m => Handle -> Path Abs File -> m ()
    appendH h path =
        withAbsFile path ReadMode (LBS.hGetContents >=> LBS.hPut h)
    appendAll :: [Path Abs File] -> Handle -> IO ()
    appendAll paths = forM_ paths . appendH


-- | Utility function for `withFile` but with an absolute path
withAbsFile :: MonadIO m => Path Abs File -> IOMode -> (Handle -> IO ()) -> m ()
withAbsFile path mode action =
    liftIO $ withFile (Path.fromAbsFile path) mode action
