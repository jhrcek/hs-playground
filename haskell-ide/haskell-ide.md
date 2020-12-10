---
title: Haskell IDE tooling
author: Jan Hrček
patat:
    incrementalLists: true
---

# Haskell IDE tooling

- GHCi
- ghcid
- haskell-language-server (in Visual Studio Code)
    - features demo
    - setup
    - troubleshooting
    - installation from source

---

# GHCi

- Glassgow Haskell Compiler - interactive
    - Haskell's REPL (Read Evaluate Print Loop)
- Load your project's modules into GHCi, play with functions
- Reload after change with `:reload` (or `:r`) command
- Simple - no additional tooling required apart from ghc
- Fast - only (re)loads necessary minimum
- How to load project components available in GHCi?
    - `stack ide targets` - list all project components
    - `stack repl TARGET(s)` - load selected component(s) into ghci
    - `stack ghci` is synonym for `stack repl`
    - `cabal repl TARGET(s)` - if you use cabal
- Disadvantage: have to reload the code manually after each change

---

# ghcid

- "GHCi as a daemon" or "GHC + a bit of an IDE"
- Installation: `stack install ghcid`
- Usage with simple project
    - run `ghcid` in the root of your haskell project
- Usage with more complex project
    - Find a command to load the code of interest into GHCi
    - Run `ghcid -c'COMMAND'
- Demo: editing haskell script with instant feedback:
    1. find a command to load your code in ghci
        `stack repl --package optics-core script.hs`
    2. run ghcid with that command
        `ghcid -c'stack repl --package optics-core script.hs'`
- Must read: [ghcid for the win!](https://www.parsonsmatt.org/2018/05/19/ghcid_for_the_win.html)

---

# haskell-language-server (aka hls)

- *haskell-language-server*
    - the community supported implementation of *Language Server Protocol*
- Beginner setup:
    - install *Haskell* extension in vscode
    - it will download haskell-language-server automatically and run it with your codebase
- Links
    - [VSCode Haskell extension](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
    - [haskell-language-server](git@github.com:haskell/haskell-language-serve)
    - [Language Server Protocol](https://microsoft.github.io/language-server-protocol/)

---

# hls - project configuration  - hie.yaml

- Depends on the knowledge "which .hs file belongs to which project component"
- Relies on `hie.yaml` file to get this information
- Automatically generates hie.yaml ("implicit cradle")
- You can use [implicit-hie](git@github.com:Avi-D-coder/implicit-hie.git) to generate hie.yaml and commit it to version control
    
```bash
git clone git@github.com:Avi-D-coder/implicit-hie.git
cd implicit-hie
stack install # puts gen-hie binary on your path
#in your Haskell project run:
gen-hie > hie.yaml
# commit hie.yaml in your version control
# regenerate it when the structure of your project changes (when you add new exe/lib/test component to your .cabal file)
```

---

# hls - troubleshooting - GHC API version

- given `haskell-language-server` binary is just compatible with one specific version of GHC
- The version used for given project is usually determined ... 
    - in stack by stackage snapshot (in stack.yaml)
    - in cabal by project configuration (in cabal.project)
- If your project uses different GHC version haskell-language-server won't work with it
- Solution:
    - rely on vscode haskell extension to download appropriate version of hls (for your project)
    - install hls from source and use haskell-language-server-wrapper

```bash
git clone git@github.com:haskell/haskell-language-server.git
cd haskell-language-server
stack ./install.hs # will print a list of things you can install
stack ./install.hs hls-8.8.4 # e.g. install haskell-language-server for GHC 8.8.4
```

---

# hls - troubleshooting

- Run `haskell-language-server` on command line in your project
    - tries to load all the hs files within current directory 
    - will warn you which ones it failed to load
- in vscode open panel 
    - basic check: View > problems
    - advanced: enable logging for haskell-language-server
        - View > Extensions > Haskell > Extension Settings
            - set path to log file
            - switch "Haskell > Trace: Server" to "messages" to see JSON RPC communication between haskell-language-server and haskell vscode client extension

