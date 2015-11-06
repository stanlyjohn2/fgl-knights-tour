# Knights tour in Haskell

## Prerequisites

* [System.Random](http://hackage.haskell.org/package/random)
* [FGL Library](http://hackage.haskell.org/package/fgl)

## Usage

```haskell
knights_tour 8 8
-- if not found (very unlikely for boards smaller than 80x80), try:
let (found, path, n_tries) = knights_tour_ext 8 8 1 16 1
```
