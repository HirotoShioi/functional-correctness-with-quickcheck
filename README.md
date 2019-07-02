# functional-correctness-with-quickcheck

https://scrapbox.io/haskell-quickcheck/

## How to build this project

You can use [stack](https://docs.haskellstack.org/en/stable/README/) to build
the project

```terminal
stack build ghci
```

### Turning off pretty-printing

I'm using [pretty-simple](http://hackage.haskell.org/package/pretty-simple) library
for the repl. If this is not preffered for you, please go to `.ghci` file and delete
these lines

```
:set -package pretty-simple
import Text.Pretty.Simple (pPrint)
:set -interactive-print pPrint
```