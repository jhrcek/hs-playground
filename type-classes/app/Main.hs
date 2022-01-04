-- {-# OPTIONS_GHC -ddump-simpl
--                 -dsuppress-idinfo
--                 -dsuppress-coercions
--                 -dsuppress-type-applications
--                 -dsuppress-uniques
-- #-}

module Main where

import qualified Example1 as E1
import qualified Example2 as E2
import qualified Example3 as E3
import qualified Example4 as E4
import qualified Example5 as E5


main :: IO ()
main = do
    E1.handler
    E2.handler putStrLn getLine E2.incrementCounterIO
    E2.handler (\_ -> pure ()) (pure "Jan") (pure ())
    E3.handler E3.consoleIO E3.counterIO
    E4.handler E4.consoleIO E4.counterIO
    E5.handler
    -- Pure implementation
    -- Will print  ((), E5.S ["Tom"] ["What's your name?", "Hello Jan"] 1)
    print $ E5.runApp2 E5.demoState E5.handler
