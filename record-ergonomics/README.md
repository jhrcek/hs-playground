# record-ergonomics

Different approaches to ergonomics of constructing values of large records.
Based on https://github.com/brendanhay/amazonka/issues/762


- Optional vs. Mandatory
- Has / doesn't have default

## 1. standard record syntax

+ Allow you to specify everything unambiguously
- Too verbose
- You have to remember/find the default values

```haskell
foo = 
  Foo
    { a = 1
    , b = Nothing
    , c = 123 -- You'd have to remember this, or float out defaultFooC in codegen, etc.
    }
```

## 2. "Smart" constructors

+ Only specify what's required
- order of parameters becomes significant

```haskell
data Bar = Bar
  { a :: Int       -- ^ Required.
  , b :: Maybe Int -- ^ Optional.
  , c :: Int       -- ^ Default to 123.
  , d :: Int       -- ^ Required.
  , e :: Int       -- ^ Required.
  }

newBar :: Int -> Int -> Int -> Bar
newBar a d e = Bar { a, b = Nothing, c = 123, d, e }
```

## 3. Named parameters
+ Solves parameter order issue (no issues with swapping params of the same type)
- Usage of overloaded labels + custom operators isn't very ergonomic

TODO play with named library?
https://hackage.haskell.org/package/named-0.3.0.1

## 4. Newtype wrappers for each param

4.a Newtype for every single field/Type
4.b Generate (sub)records based on required
    - 2 x n datatypes

## 5. Record pattern synonyms

## 6. HKD
+ named + freely ordered parameters + punning

