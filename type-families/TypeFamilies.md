# Type families                                                       
- a function from types to types                                          
- Computation specified by type family is executed at compile time
- If computation specified by TF fails, you get type error
- You can evaluate type families in GHCi using `:kind!` command

# Clarifying DataKinds
- DataKinds is often used in type-level programming, because it makes kind system more expressive
- so Haskell can do more fine grained checks that our type-level programs make sense

 Standard Haskell                   |  With DataKinds
                                    |
Kind:          Type (= * )          |  Bool            Nat           Symbol (not String)
                                    |
Type:     Bool     String = [Char]  | 'True, 'False    1, 2, 3       "hello"
                                    |
Value:    True     "hello"          |    (these types have no values)


# Kinds of type families:
- Closed
- Open
     - top-level
     - nested in type class (those are called "Associated types")


# Examples
- base        https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Type-Bool.html
- elm-street  https://hackage.haskell.org/package/elm-street-0.2.0.0/docs/Elm-Generic.html
- servant     https://hackage.haskell.org/package/servant-server-0.19.1/docs/Servant-Server.html#t:HasServer
              https://hackage.haskell.org/package/servant-0.19/docs/Servant-Links.html#t:HasLink
- amazonka    https://hackage.haskell.org/package/amazonka-1.6.1/docs/Network-AWS.html#t:AWSRequest


# Read thinking with types - available in Holmusk library
