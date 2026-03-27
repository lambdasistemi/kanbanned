{- |
Module      : Main
Description : Unit test entry point
-}
module Main (main) where

import Kanbanned.GitHub.TypesSpec qualified
import Kanbanned.Terminal.InputSpec qualified
import Test.Hspec.Runner (defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig $ do
    Kanbanned.Terminal.InputSpec.spec
    Kanbanned.GitHub.TypesSpec.spec
