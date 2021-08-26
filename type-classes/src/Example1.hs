module Example1 where


handler :: IO ()
handler = do
    putStrLn "What's your name?"
    name <- getLine
    putStrLn $ "Hello " <> name
    incrementCounter


incrementCounter :: IO ()
incrementCounter = putStrLn "I'm incrementing counter in DB!"
