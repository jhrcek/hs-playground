module Main where

main :: IO ()
main = do
   print $ isZeroArg Zero
   print $ isZeroArg (One 1)
   print $ isZeroArg (Two True 'a')
   print $ isZeroArg (Nothing :: Maybe ())
   print $ isZeroArg (Just ())

data Foo
    = Zero
    | One Int
    | Two Bool Char
    deriving (Show)

class ZeroArgConstructor a where
    isZeroArg :: a -> Bool

instance ZeroArgConstructor Foo where
    isZeroArg Zero      = True
    isZeroArg (One _)   = False
    isZeroArg (Two _ _) = False

instance ZeroArgConstructor (Maybe a) where
    isZeroArg Nothing  = True
    isZeroArg (Just _) = False
