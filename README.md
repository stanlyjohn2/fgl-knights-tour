# Knights tour in Haskell

## Prerequisites

* [GHC](https://www.haskell.org/ghc/) (>=7.8)
* [System.Random](http://hackage.haskell.org/package/random)
* [FGL Library](http://hackage.haskell.org/package/fgl)

## Installation Instruction

Download the compressed source files of the two libraries from the above links, and follow the
instructions given [here](https://wiki.haskell.org/Cabal/How_to_install_a_Cabal_package).

## Usage

Run `ghci knights.hs` from the project directory, and try out:

```haskell
knights_tour 8 8
-- if not found (very unlikely for boards smaller than 80x80), try:
let (found, path, n_tries) = knights_tour_ext 8 8 1 16 1
```
