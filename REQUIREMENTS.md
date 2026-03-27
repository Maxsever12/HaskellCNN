# Requirements

## Toolchain
- GHC 9.6.7
- Cabal (v2 commands)

## Haskell libraries
- random == 1.3.1

## Project-local package environment
This project uses a local GHC package environment file:
- `.ghc.environment.x86_64-linux-9.6.7`

To (re)install dependencies locally from the project root:

```bash
cabal update
cabal install random --lib --package-env .
```

## Verify setup
From the project root, run:

```bash
printf ':m + System.Random\n:quit\n' | ghci -ignore-dot-ghci
```

Expected behavior:
- GHCi reports it loaded the local `.ghc.environment.x86_64-linux-9.6.7` file.
- No import error for `System.Random`.
