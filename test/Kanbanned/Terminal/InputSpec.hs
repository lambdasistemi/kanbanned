{- |
Module      : Kanbanned.Terminal.InputSpec
Description : Tests for terminal input parsing
-}
module Kanbanned.Terminal.InputSpec (spec) where

import Kanbanned.Terminal.Input
    ( Key (..)
    , Modifier (..)
    , noMod
    )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Kanbanned.Terminal.Input" $ do
    describe "noMod" $ do
        it "has no modifiers set" $ do
            modCtrl noMod `shouldBe` False
            modAlt noMod `shouldBe` False
            modShift noMod `shouldBe` False

    describe "Key" $ do
        it "supports equality" $ do
            KeyChar 'a' `shouldBe` KeyChar 'a'
            KeyEnter `shouldBe` KeyEnter
            KeyUp `shouldBe` KeyUp
