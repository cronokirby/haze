module Haze
    ( runHaze
    ) 
where

import Relude

import System.Environment (getArgs)

import Haze.Client (launchClient)


runHaze :: IO ()
runHaze = do
    args <- getArgs
    case args of
        []     -> putStrLn "Please give me a file"
        file:_ -> launchClient file
