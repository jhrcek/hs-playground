module BasicSpec where

import Control.Concurrent (threadDelay)
import Lib (someFunc)
import Test.Hspec


spec :: Spec
spec =
    describe "Demo " $ do
        it "should be true" True
        it "2" True
        it "3" True
