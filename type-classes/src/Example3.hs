{-# LANGUAGE NamedFieldPuns #-}

module Example3 where


handler :: Console -> Counter -> IO ()
handler console cntr = do
    printLine console "What's your name?"
    name <- readLine console
    printLine console $ "Hello " <> name
    incrementCounter cntr


data Console = Console
    { printLine :: String -> IO ()
    , readLine :: IO String
    }


data Counter = Counter
    { incrementCounter :: IO ()
    }


consoleIO :: Console
consoleIO =
    Console
        { printLine = putStrLn
        , readLine = getLine
        }


counterIO :: Counter
counterIO = Counter incrementCounterIO


incrementCounterIO :: IO ()
incrementCounterIO = putStrLn "I'm incrementing counter in DB!"