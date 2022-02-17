---
title: GHC Generics
author: Jan Hrček
---

# GHC Generics

- Generics in Java
- Core idea (from 10 000 ft)
- Isomorphisms
- When to use Generics
- Generic typeclass
- associated type family Rep
- implementing functionality relying on generics

---

# Generics in Java

- Generics in Java have nothing to do with Generics in Haskell
- Generics in Java correspond to Parametric polymorpish in haskell
- Best to forget about generics as you know them from Java

---

# Core idea from 10 000 ft

- Standard algebraic data types are not sufficiently "uniform"
- `Generic` type class gives you a way to convert your custom types to more "uniform" representation (via `Rep` type family)
- Library authors implement functionality that works with these "uniform" representations
- You get access to the library functionality by adding `deriving Generic` to your data type definition

---

# When to use Generics

- idea from `Thinking with Types` from Sandy Maguire
- spectrum:
    - parametric polymorphism - works the same **for all** types
    - ...
    - ad hoc polymorphism - Type Classes - each type has its **unique implementation** of the type class

- There are many examples of functionality that's kindof sortof similar for all types, but differs in details
- implementing it feels like boilerplate
    - conversion to/from JSON
    - binary serialization/deserialization
    - conversion from/to database representation
- In these cases Generics help us remove the boilerplate

---

# Isomorphisms

- We say types `A` and `B` are isomorphic, if there are two functions
`aToB :: A -> B` and `bToA :: B -> A`
such that `bToA . aToB == identity` and `aToB . bToA == identity`
- examples:
    - `Maybe a` is isomorphic to `Either () a`
    - `Natural` is isomorphic to `[()]`
    - `a` is isomorphic to `(a -> r) -> r`
    
---

# Playing with the Generic type class + Rep type family


TODO


---

# References

- [Datatype-Generic Programming by Andres Löh](https://www.youtube.com/watch?v=pwnrfREbhWY)
- [generics-eot](https://hackage.haskell.org/package/generics-eot-0.4.0.1) - lightweight "toy" library demonstrating the basic ideas
