# functional-correctness-with-quickcheck

Working implementation of collections of effective QuickCheck tests based on the
[paper](./paper.pdf) written by John Hughes, founder of QuickCheck.

## How to build this project

You can use [stack](https://docs.haskellstack.org/en/stable/README/) to build
the project

```terminal
stack build ghci
```

### Turning off pretty-printing

I'm using [pretty-simple](http://hackage.haskell.org/package/pretty-simple) library
for the repl. If this is not preferred for you, please go to `.ghci` file and delete
these lines

```
:set -package pretty-simple
import Text.Pretty.Simple (pPrint)
:set -interactive-print pPrint
```