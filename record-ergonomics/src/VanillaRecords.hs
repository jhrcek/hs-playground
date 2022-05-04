{-# LANGUAGE NamedFieldPuns #-}

module VanillaRecords where


{- |

We can classify each field of the record along two dimensions:

1. Is it required?
2. Does it have a sensible default value?


required?  |   has default ?   |    notes
-----------+-------------------+--------------
   YES             YES            library can initialize the field to "default"
   YES             NO             User has to provide the desired value when constructing the record
   NO              YES            library can initialize the field to "Just default"
   NO              NO             library can initialize the field to "Nothing"
-}

data Foo = Foo
    { a :: Int       -- ^ Required.
    , b :: Maybe Int -- ^ Optional.
    , c :: Int       -- ^ The service definition explicitly states that field 'c' is required,
                     --   but has a default of 123.
    }


foo :: Foo
foo =
    Foo
        { a = 1
        , b = Nothing
        , c = 123 -- You'd have to remember this, or float out defaultFooC in codegen, etc.
        }


newFoo :: Int -> Foo
newFoo a = Foo{a, b = Nothing, c = 123}


data Bar = Bar
    { ba :: Int        -- ^ Required.
    , bb :: Maybe Int  -- ^ Optional.
    , bc :: Int        -- ^ Default to 123.
    , bd :: Int        -- ^ Required.
    , be :: Int        -- ^ Required.
    } deriving Show


-- Problem: order of arguments becomes significant
newBar :: Int -> Int -> Int -> Bar
newBar ba bd be = Bar{ba, bb = Nothing, bc = 123, bd, be}
