module Haze
    ( runHaze
    )
where

import           Relude

import           System.Environment             ( getArgs )

import           Haze.Client                    ( launchClient )


runHaze :: IO ()
runHaze = do
    args <- getArgs
    case args of
        file : dir : _ -> launchClient file dir
        _       -> putStrLn "Please give me a file and a directory"
