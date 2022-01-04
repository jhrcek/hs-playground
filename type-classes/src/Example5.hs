{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

-- {-# OPTIONS_GHC -ddump-simpl
--                 -dsuppress-idinfo
--                 -dsuppress-coercions
--                 -dsuppress-type-applications
--                 -dsuppress-uniques #-}

module Example5 where

import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State.Strict (MonadState (get, put), StateT (..), modify)


handler :: (Console m, Counter m) => m ()
handler = do
    printLine "What's your name?"
    name <- readLine
    printLine ("Hello " <> name)
    incrementCounter


appHandler :: App ()
appHandler = App handler


class Monad m => Console m where
    printLine :: String -> m ()
    readLine :: m String


class Monad m => Counter m where
    incrementCounter :: m ()


instance Console IO where
    printLine = putStrLn
    readLine = getLine


instance Counter IO where
    incrementCounter = incrementCounterIO


newtype App a = App {unApp :: IO a}
    deriving (Functor, Applicative, Monad, Console, Counter) via IO


newtype App2 a = App2 {unApp2 :: StateT S Identity a}
    deriving (Functor, Applicative, Monad) via StateT S Identity


data S = S
    { inputLines :: [String]
    , outputLine :: [String]
    , counter :: Int
    }
    deriving (Eq, Show)


instance Console App2 where
    printLine str = App2 $ do
        modify (\s -> s{outputLine = outputLine s <> [str]})


    readLine = App2 $ do
        s <- get
        case inputLines s of
            [] -> pure "sorry no input lines"
            str : ss -> put (s{inputLines = ss}) >> pure str


instance Counter App2 where
    incrementCounter = App2 $ modify (\s -> s{counter = counter s + 1})


runApp2 :: S -> App2 a -> (a, S)
runApp2 initState action =
    runIdentity $ runStateT (unApp2 action) initState


demoState :: S
demoState =
    S
        { inputLines = ["Jan", "Tom"]
        , outputLine = []
        , counter = 0
        }


incrementCounterIO :: IO ()
incrementCounterIO = putStrLn "I'm incrementing counter in DB!"
