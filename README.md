# hs-playground

Playground for playing around with all kinds of stuff related to Haskell.

# Load examples into GHCi

You can list all available project components by

```bash
$ stack ide targets
hs-playground:exe:generics-demo
hs-playground:exe:monad-transformers-demo
hs-playground:exe:servant-demo
```

Pick one and of the available targets and load it into repl. For example
```bash
stack repl hs-playground:exe:monad-transformers-demo
```
