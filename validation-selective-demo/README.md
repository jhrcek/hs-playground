# validation-selective-demo

Demo of how [validation-selective](https://hackage.haskell.org/package/validation-selective)
can be used to accumulate validation errors.

High level summary:
The Applicative instance of Validation datatype (unlike Applicative instance of Either)
accumulates errors, which makes it possible to see all errors in the data structure we're validating at once.