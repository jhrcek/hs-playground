---
title: HSpec testing tips
author: Jan Hrček
...

# HSpec testing tips

- passing arguments to tests
- what constitutes a test
- setup & teardown
- some antipatterns

---

# Passing arguments to tests

- When you run `stack test ...` every `test-suite XYZ` in your cabal file gets compiled into executable `XYZ` and executed
- Popular test frameworks (hspec, tasty, hedgehog, ...) have bunch of useful CLI flags
- Passing flags to test frameworks:
    - directly to test executable
    - via stack's CLI (`--test-arguments` / `--ta`)
    - via stack.yaml

---

# Running tests

- manually
- faster: stack's `--file-watch` flag
- fastest: use ghcid

---

# Running tests (2)

- running `stack test` every time you change something is tedious and slow
- somewhat faster: `stack test --fast --file-watch` - recompiles on save

---

# Running tests (3)

- Even faster: use `ghcid`
- Steps to assemble proper `ghcid` command:
    1. list project components `stack ide targets`
    2. create and test command to load components of interest in ghci: `stack ghci COMPONENT1 --main-is COMPONENT2`
    3. pass command from step 2 to `ghcid` : `ghcid --command='stack ghci COMPONENT1 --main-is COMPONENT2' `
    4. add `--run`
        - by default will invoke `main` as if you typed `main` in `GHCi`
        - if the main in scope in GHCi is test suite's `main` (influenced by `--main-is`) the test suite will be executed

---

# Hspec spec items (`it`)

- what can you put behind `it`
    - Bool
    - QuickCheck Property
    - Expectation (== any `IO ()` action)

---

# Hspec hooks - setup & tear down resources

- running custom setup/teardown action
    - before/after/arount every spec item / subtree
- ensure some cleanup/teardown action runs regardless of test failures
- some hoos pass argumetns to spec items
- example:
```haskell
before :: IO a -> SpecWith a -> Spec
```
  ioa = `IO` action that runs before every spec item in spec
  every spec item gets value of type `a` created by that action
  `SpecWith a` = every `it` it contains must be function accepting value of type `a`

---

# Testing antipatterns

- test suite should be idempotent
    - running tests should not leave resources behind (e.g. data in DB) that influence following test runs
- depending on data created outside of tests
    - tests should control creation / destruction of data they need
- depending on data created by other tests
    - e.g. test1 creates project, test2 deletes project created by test1
    - tests should not depend on data created by previously running tests
    - if they do:
        - you cannot run individual tests in isolation
        - you cannot run tests in random order
        - you cannot run tests in parallel
- improper scoping of setup
    - resources should be created
- avoid "shotgun cleanup"
    - e.g. `TRUNCATE table1, table2, ...`
    - bad, because this can delete data created by other tests

---

# HSpec User manual

See hspec manual at https://hspec.github.io/
