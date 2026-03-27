{- |
Module      : Main
Description : Unit test entry point
-}
module Main (main) where

import Test.Hspec (hspec, describe, it, shouldBe)

main :: IO ()
main = hspec $ do
    describe "Kanbanned" $ do
        it "placeholder" $ do
            True `shouldBe` True
