module Example2 where


handler :: (String -> IO ()) -> IO String -> IO () -> IO ()
handler printLine readLine incrementCounter = do
    printLine "What's your name?"
    name <- readLine
    printLine $ "Hello " <> name
    incrementCounter


incrementCounterIO :: IO ()
incrementCounterIO = putStrLn "I'm incrementing counter in DB!"