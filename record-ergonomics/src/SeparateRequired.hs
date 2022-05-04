{-# LANGUAGE DuplicateRecordFields, NamedFieldPuns#-}
module SeparateRequired where

newtype BazRequired = BazRequired
  { bazA :: Int
  }

data Baz = Baz
  { a :: Int       -- ^ Required.
  , b :: Maybe Int -- ^ Optional.
  , c :: Int       -- ^ Default to 123.
  } deriving Show

newBaz :: BazRequired -> Baz
newBaz BazRequired {bazA} = Baz { a = bazA, b = Nothing, c = 123 }


myBaz :: Baz
myBaz = newBaz BazRequired{bazA = 1}