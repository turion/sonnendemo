# README


[![Build Status](https://travis-ci.org/turion/sonnendemo.svg?branch=master)](https://travis-ci.org/turion/sonnendemo)

This is a simple example game for the gloss bindings
for [Rhine](https://github.com/turion/rhine),
an asynchronous functional reactive programming library.

## Installation

I'm assuming that you have a regular Haskell development environment, on any platform.

```
git clone https://github.com/turion/sonnendemo
cd sonnendemo
```

### With cabal

You will probably need to install OpenGL development libraries and FreeGLUT.

```
cabal sandbox init
cabal install --only-dependencies
cabal build
```

### With stack

You have to have [`nix`](https://nixos.org/nix/) installed.
`stack` uses `nix` in order to automatically install all OpenGL backend packages.

```
stack build
```

## Play

* __With cabal__: 
To play it, simply run `cabal run sonnendemo` for a GUI version,
or `cabal run sonnendemo-console` for a console version.

* __With stack__:
Run `stack exec sonnendemo` for a GUI version,
or `stack exec sonnendemo-console` for a console version.

### The game (`SonnenModel.hs`)

* The objective of the game is to drink coffee.
  You cannot lose, or die.
* The coffee machine runs on renewable energy, of course,
   and is powered by a little solar plant and a wind turbine.
* The energy is stored in a battery.
  When there is a sufficient charge level in it, you can make a coffee, and then drink it.
* Under usual conditions, the battery can never be fully charged or emptied.
  There is always a little reserve charge and a little extra capacity left.
  _This is on purpose._ Sometimes, the wind will be very strong, and extra energy needs to be absorbed.
  In other times, the wind is still, and energy needs to be delivered from the battery to the grid.
  These things happen automatically. You can simply make coffees and marvel while it happens.

### GUI version (`MainGloss.hs`)

1. If there is enough energy in the battery (and a little reserve),
  a green check mark will appear next to the coffee cup.
  (Otherwise, a red cross will be displayed.)
  You can then brew a coffee by clicking on the cup.
2. To drink the coffee, click on the cup once it's full.
3. Repeat :)
4. Exit by pressing `Escape`.

Here is a screenshot:

![screenshot](screenshot.png)

### Console version (`MainConsole.hs`)

All relevant information appears on the console.
Brew and drink coffee by pressing `Enter`.
