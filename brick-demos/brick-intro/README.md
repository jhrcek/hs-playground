---
patat:
  slideLevel: 2
...

# Introduction to Brick (TUI library)

Introduction to [brick](https://github.com/jtdaugherty/brick) library for building TUIs (Terminal User Interfaces).


## Example TUI programs?

  - tig
  - dive
  - ghcup tui

## Learn about brick from demos in brick repo

The best way to learn about what brick can do is to clone the repo
and run the example programs:

```sh
git clone git@github.com:jtdaugherty/brick.git
cd brick
grep executable brick.cabal
# ^ this will list 32 demo executables.
# pick one and run as follows (you need to enable demos cabal flag)
cabal run --flag demos brick-hello-world-demo
```

## Few examples of what brick can do

```sh
cabal run --flag demos brick-dynamic-border-demo
cabal run --flag demos brick-list-demo
cabal run --flag demos brick-mouse-demo
cabal run --flag demos brick-form-demo
cabal run --flag demos brick-file-browser-demo
```

## TUIs in brick
Analogies between Brick and "The Elm Architecture".

A brick application consists of:
- Model - data type representing state of the app
- update - function receiving events (key pressed, mouse clicked etc.) and modifying Model/run effects accordingly
- view - function that takes Model and renders view 
- styling - change color of text / background / style of text

## Live coding examples
- hello world
- counter
- data-explorer

# Q & A