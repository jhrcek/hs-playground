{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators     #-}

module Main where
import GHC.Generics

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
    deriving (Generic, Show, ZeroArgConstructor)

class ZeroArgConstructor a where
    isZeroArg :: a -> Bool

    default isZeroArg :: (Generic a, GZeroArgConstructor (Rep a)) => a -> Bool
    isZeroArg = gIsZeroArg . from

instance ZeroArgConstructor (Maybe a)

class GZeroArgConstructor f where
    gIsZeroArg :: f x -> Bool

instance GZeroArgConstructor U1 where
    gIsZeroArg _ = True

instance GZeroArgConstructor (K1 i c) where
    gIsZeroArg _ = False

instance (GZeroArgConstructor a, GZeroArgConstructor b) => GZeroArgConstructor (a :+: b) where
    gIsZeroArg (L1 x) = gIsZeroArg x
    gIsZeroArg (R1 x) = gIsZeroArg x

instance (GZeroArgConstructor a, GZeroArgConstructor b) => GZeroArgConstructor (a :*: b) where
    gIsZeroArg _ = False

instance GZeroArgConstructor f => GZeroArgConstructor (M1 i c f) where
    gIsZeroArg (M1 x) = gIsZeroArg x
