{-# LANGUAGE NamedFieldPuns #-}

module Example4 where


handler :: Monad m => Console m -> Counter m -> m ()
handler console cntr = do
    printLine console "What's your name?"
    name <- readLine console
    printLine console ("Hello " <> name)
    incrementCounter cntr


data Console m = Console
    { printLine :: String -> m ()
    , readLine :: m String
    }


data Counter m = Counter
    { incrementCounter :: m ()
    }


consoleIO :: Console IO
consoleIO =
    Console
        { printLine = putStrLn
        , readLine = getLine
        }


counterIO :: Counter IO
counterIO = Counter incrementCounterIO


incrementCounterIO :: IO ()
incrementCounterIO = putStrLn "I'm incrementing counter in DB!"
