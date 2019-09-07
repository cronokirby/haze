module Haze
    ( runHaze
    )
where

import           Relude


import           Haze.Client                    ( launchClient )
import           Haze.Config                    ( parseConfig )

_test :: Int
_test = 3

runHaze :: IO ()
runHaze = parseConfig >>= launchClient
