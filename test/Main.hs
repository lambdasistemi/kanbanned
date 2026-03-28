{- |
Module      : Main
Description : Unit test entry point
-}
module Main (main) where

import Kanbanned.GitHub.TypesSpec qualified
import Test.Hspec.Runner (defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig $ do
    Kanbanned.GitHub.TypesSpec.spec
