# Knights tour in Haskell

## Prerequisites

* [System.Random](http://hackage.haskell.org/package/random)
* [FGL Library](http://hackage.haskell.org/package/fgl)

## Usage

```haskell
let gen1 = mkStdGen 1
let board = mk_board 8 8
let (path, found, gen2) = hop_seq board gen1
-- if not found, try:
let (path, found, gen3) = iter_find board 10 gen2
```
