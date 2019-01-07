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

import Data.Array (Array, (!), array)
import Path (Path, File, (</>), (<.>), fromRelFile, parseRelFile)

import           Haze.Tracker                   ( FileInfo(..)
                                                , SHAPieces(..)
                                                )


-- Maybe make this take a base file path?
{- | Write all pieces to disk, given a file specification to follow

For pieces part of an incomplete file, the data is saved as "piece-1.bin".
For pieces that overlap between 2 files, the piece is saved in
"fileA-start.bin" for the piece starting "fileA", and "fileB-end.bin"
for the piece ending "fileB". 
-}
writePieces :: MonadIO m => FileInfo -> SHAPieces -> [(Int, ByteString)] -> m ()
writePieces (SingleFile path fileLength _) (SHAPieces pieceSize _) pieces = do
    piecePaths <- forM [0..maxPiece] $ liftIO $ parseRel
  where
    makePiecePath :: Int -> Path Rel File ->
    let maxPiece = fileLength `div` pieceSize

        pieceNames = array (0, maxPiece) 
                    ["piece-" ++ show i ++ ".bin" | i <- [0..maxPiece]]
    forM_ pieces $ \(piece, bytes) -> do
        -- This really shouldn't ever throw
        piecePath <- liftIO $ parseRelFile ("piece-" ++ show piece ++ ".bin")
        writeFileBS (fromRelFile piecePath) bytes
    
    