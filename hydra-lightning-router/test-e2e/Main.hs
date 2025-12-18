module Main (main) where

import Spec qualified
import Test.Hspec.Runner (hspec)
import Prelude (IO)

main :: IO ()
main = hspec Spec.spec
