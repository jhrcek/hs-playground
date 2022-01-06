--{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module Main where

import qualified BasicSpec
import qualified HooksSpec
import qualified ItSpec
import Test.Hspec (hspec)


main :: IO ()
main = hspec $ do
    BasicSpec.spec
    ItSpec.spec
    HooksSpec.spec
