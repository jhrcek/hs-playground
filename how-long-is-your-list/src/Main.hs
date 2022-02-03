{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.Foldable (for_)
import Data.Functor.Identity (Identity (Identity))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Monoid ()
import Data.Proxy (Proxy (..))


{-
Based on
http://jackkelly.name/blog/archives/2022/01/15/how_long_is_your_list/

All of these can be thought of as list-like types that can contain certain number of elements.
How many elements can they hold?

0       1         infinity

Y       Y         Y           [a]
Y       Y         -           Maybe a
Y       -         -           Proxy a
-       Y         -           Identity a
-       Y         Y           NonEmpty a
-       -         Y           Stream a

-}

main :: IO ()
main = do
    -- []
    for_ [1] print

    -- Maybe
    for_ (Nothing :: Maybe ()) print
    for_ (Just 2) print

    -- Identity
    for_ (Identity 20) print

    -- NonEmpty
    for_ (30 :| [40]) print

    -- -- Proxy
    for_ (Proxy :: Proxy ()) print


-- for_ infiniteStream print

data Stream a = Stream a (Stream a) deriving (Functor, Foldable)


infiniteStream :: Stream Integer
infiniteStream = Stream 1 infiniteStream
