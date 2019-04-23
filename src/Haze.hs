module Haze
    ( runHaze
    )
where

import           Relude


import           Haze.Client                    ( launchClient )
import           Haze.Config                    ( parseConfig )


runHaze :: IO ()
runHaze = parseConfig >>= launchClient
