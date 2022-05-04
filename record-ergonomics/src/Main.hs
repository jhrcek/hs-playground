module Main where

import qualified HKD
import qualified NamedParameters
import SeparateRequired
import qualified VanillaRecords


main :: IO ()
main = do
    print $ VanillaRecords.newBar 1 2 3
    print NamedParameters.bar
    print $ SeparateRequired.newBaz (BazRequired{bazA = 1})
    print HKD.myDelete