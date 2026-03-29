{- |
Module      : Kanbanned.GitHub.TypesSpec
Description : Tests for GitHub types
-}
module Kanbanned.GitHub.TypesSpec (spec) where

import Data.Aeson (decode, encode)
import Kanbanned.GitHub.Types
    ( KanbanStatus (..)
    , statusToText
    , textToStatus
    )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "Kanbanned.GitHub.Types" $ do
    describe "KanbanStatus" $ do
        it "converts to text" $ do
            statusToText Backlog `shouldBe` "Backlog"
            statusToText WIP `shouldBe` "WIP"
            statusToText Done `shouldBe` "Done"

        it "parses from text" $ do
            textToStatus "Backlog" `shouldBe` Just Backlog
            textToStatus "wip" `shouldBe` Just WIP
            textToStatus "done" `shouldBe` Just Done
            textToStatus "invalid" `shouldBe` Nothing

        it "roundtrips through JSON" $ do
            let statuses = [Backlog, WIP, Done]
            mapM_
                ( \s ->
                    decode (encode s)
                        `shouldBe` Just s
                )
                statuses
