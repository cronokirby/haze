module Haze
    ( runHaze
    )
where

import           Relude


import           Haze.Client                    ( launchClient )
import           Haze.Config                    ( parseConfig )

test :: Int
test = 3

runHaze :: IO ()
runHaze = parseConfig >>= launchClient
