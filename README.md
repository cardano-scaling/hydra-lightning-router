# hydra-lightning-router

Demonstration of routing a payment between two hydra heads using the
[htlc](https://github.com/cardano-scaling/htlc) validator.


## Running

```
# In one terminal
nix run .#demo

# In another
cabal run hydra-lightning-router
```

## Smoke Tests

```
nix develop
cabal test all
```
