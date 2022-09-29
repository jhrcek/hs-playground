module Main (main) where

import Counter qualified
import HelloWorld qualified

main :: IO ()
main = do
    HelloWorld.main
    --Counter.main
