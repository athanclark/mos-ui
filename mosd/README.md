# mosd


## Prerequisites

You'll need the build tool [Stack](https://haskellstack.org) to build the haskell code.


## Building

```
# this will fetch the correct compiler and dependencies, building the executable
stack build --no-system-ghc --install-ghc
```

After this is done, there will be an executable in `.stack-work/install/<arch>/lts-9.6/8.0.2/bin/mosd`.
