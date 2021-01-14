---
title: Algebra-Driven Design - review
author: Jan Hrček
patat:
    incrementalLists: true
---

# Algebra-Driven Design

- By Sandy Maguire
    - author of [Thinking with types](https://thinkingwithtypes.com/), and [polysemy](https://hackage.haskell.org/package/polysemy)
- The book has 327 pages
- Free sample of the first 85 pages available from the [book's webpage](https://algebradriven.design/)
- A package on hackage with accompanying code [algebra-driven-design](http://hackage.haskell.org/package/algebra-driven-design)
- "If FP is so fantastic, why hasn't it taken over the world yet?"
    - ... because we aren't yet good enough with it
    - works "in the small" but in the large, you need to deal with external systems and real-world complexity

---

# High-level summary of the contents

- Design Algebras (110 pages)
    - 2 end to end case studies
        - **Tiles**
            - an algebra for building images by subdividing space into tiles which can be rotated and flipped
        - **Scavenger hunt**
            - inspired by real world app that author has been working on
            - players run around the city, solve clues, explore new places and do challenges
    - come up with types to model your domain (DSL - Doman Specific Language)
    - think about how the building blocks relate to each other
    - the relationships between pieces give you properties that the algebra has to satisfy
    
- Derive implementation (91 pages)
    - Create initial encoding of your algebra (i.e. represent everything as data constructors)
        - naive, but obviously correct
    - Use [QuickSpec](https://hackage.haskell.org/package/quickspec) to generate [QuickCheck](https://hackage.haskell.org/package/QuickCheck) property tests
    - Implement it more efficiently
        - Property tests generated in prev. step ensure correctness

- Reference material (72 pages)
    - Tips on using QuickCheck and QuickSpec
    - Reference of common algebraic concepts
        - properties like associativity, idempotency etc.
        - structures like semigroups, monoids, functors etc.

---

# Designing Algebras

- understand the domain, the concepts and their relations before writing any code
    - "we want to sculpt our software in thought, having it mostly worked out before we ever white a line of code"
- Example from the "Tiles" case study
    - we have a type of images `Tile` and some operations
    - `cw :: Tile -> Tile` - rotate the image by 90 degrees clockwise 
    - `flipH :: Tile -> Tile -> Tile` - flip tile horizontally
    - `beside :: Tile -> Tile -> Tile` - put one image next to the other, while squashing both by 1/2 horizonally
    - `∀ (t :: Tile) . cw (cw (cw (cw t))) == t`
    - `∀ (t :: Tile). flipH (flipH t) = t`
- the final design has dozens of equations like these.
- Any implementation of the design is necessarily a solution to that system of equations

---

# Deriving Implementations

- implement naive, obviously correct solution
    - "Initial encoding" - represent terms of our algebra as data constructors
```haskell
data Tile a where
  Cw :: Tile a -> Tile a
  FlipH :: Tile a -> Tile a
  Beside :: Tile a -> Tile a -> Tile a
  ...
```
- enforce the laws by pattern matching
```haskell
flipH :: Tile a -> Tile a
flipH (FlipH t) = t
flipH t = FlipH t
```
- ... or by picking some constructors as "primitive" and expressing others in terms of primitives
```haskell
-- Clockwise rotation by 90° as primitive operation
cw :: Tile a -> Tile a
cw = Cw
-- Counter-clockwise rot. by 90° as derived operation
ccw :: Tile a -> Tile a
ccw = cw . cw . cw
```


- Use the naive implementation to generate suite of regression tests via `QuickSpec`
- create omptimized implementation using the test suite generated in previous step
