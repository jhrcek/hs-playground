# Aeson demo

```
ghcid -c'stack repl --main-is hs-playground:exe:aeson-demo' --test=":main"
```

Dump TH splices in nice readable way (http://dev.stephendiehl.com/hask/#core)
```
stack build hs-playground:exe:aeson-demo --ghc-options="-ddump-splices -dsuppress-module-prefixes -dsuppress-uniques"
```
