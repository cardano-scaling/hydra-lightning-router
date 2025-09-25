module Main (main) where

import Hydra.HTLC.Embed (htlcValidatorScript)

main :: IO ()
main = print htlcValidatorScript
