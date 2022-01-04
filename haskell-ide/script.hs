import Optics.Core


main :: IO ()
main = do
    print $ over mapped (+ 1) [1, 2, 3]
