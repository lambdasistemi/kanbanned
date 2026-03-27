{- |
Module      : Kanbanned.GitHub.Types
Description : GitHub Projects v2 domain types
-}
module Kanbanned.GitHub.Types
    ( Project (..)
    , ProjectItem (..)
    , ItemType (..)
    , StatusField (..)
    , StatusOption (..)
    , KanbanStatus (..)
    , statusToText
    , textToStatus
    ) where

import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , object
    , withObject
    , withText
    , (.:)
    , (.=)
    )
import Data.Text (Text)
import Data.Text qualified as T

-- | Kanban column status
data KanbanStatus
    = Backlog
    | WIP
    | Done
    deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Convert status to display text
statusToText :: KanbanStatus -> Text
statusToText = \case
    Backlog -> "Backlog"
    WIP -> "WIP"
    Done -> "Done"

-- | Parse status from text
textToStatus :: Text -> Maybe KanbanStatus
textToStatus t = case T.toLower t of
    "backlog" -> Just Backlog
    "wip" -> Just WIP
    "done" -> Just Done
    _ -> Nothing

instance FromJSON KanbanStatus where
    parseJSON = withText "KanbanStatus" $ \t ->
        case textToStatus t of
            Just s -> pure s
            Nothing -> fail $ "Unknown status: " <> T.unpack t

instance ToJSON KanbanStatus where
    toJSON = toJSON . statusToText

-- | GitHub Projects v2 project
data Project = Project
    { projectId :: !Text
    , projectTitle :: !Text
    , projectNumber :: !Int
    , projectItemCount :: !Int
    }
    deriving stock (Show, Eq)

-- | Type of project item
data ItemType
    = IssueItem
    | PullRequestItem
    | DraftIssueItem
    deriving stock (Show, Eq, Ord)

instance FromJSON ItemType where
    parseJSON = withText "ItemType" $ \case
        "ISSUE" -> pure IssueItem
        "PULL_REQUEST" -> pure PullRequestItem
        "DRAFT_ISSUE" -> pure DraftIssueItem
        t -> fail $ "Unknown item type: " <> T.unpack t

-- | A single item on a project board
data ProjectItem = ProjectItem
    { itemId :: !Text
    , itemTitle :: !Text
    , itemStatus :: !(Maybe KanbanStatus)
    , itemType :: !ItemType
    , itemNumber :: !(Maybe Int)
    , itemRepoName :: !(Maybe Text)
    , itemRepoOwner :: !(Maybe Text)
    , itemLabels :: ![Text]
    , itemBody :: !(Maybe Text)
    , itemDraftId :: !(Maybe Text)
    }
    deriving stock (Show, Eq)

-- | Status field metadata for a project
data StatusField = StatusField
    { sfFieldId :: !Text
    , sfOptions :: ![StatusOption]
    }
    deriving stock (Show, Eq)

-- | A single status option
data StatusOption = StatusOption
    { soId :: !Text
    , soName :: !Text
    , soStatus :: !(Maybe KanbanStatus)
    }
    deriving stock (Show, Eq)

instance FromJSON StatusOption where
    parseJSON = withObject "StatusOption" $ \o -> do
        soId <- o .: "id"
        soName <- o .: "name"
        let soStatus = textToStatus soName
        pure StatusOption{..}

instance ToJSON StatusOption where
    toJSON StatusOption{..} =
        object
            [ "id" .= soId
            , "name" .= soName
            ]
