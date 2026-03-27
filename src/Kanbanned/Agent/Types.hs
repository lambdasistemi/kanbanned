{- |
Module      : Kanbanned.Agent.Types
Description : Agent-daemon domain types
-}
module Kanbanned.Agent.Types
    ( AgentSession (..)
    , SessionState (..)
    , Repo (..)
    , WorktreeInfo (..)
    , BranchInfo (..)
    , SyncStatus (..)
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

-- | Repository identifier
data Repo = Repo
    { repoOwner :: !Text
    , repoName :: !Text
    }
    deriving stock (Show, Eq, Ord)

instance FromJSON Repo where
    parseJSON = withObject "Repo" $ \o ->
        Repo <$> o .: "owner" <*> o .: "name"

instance ToJSON Repo where
    toJSON Repo{..} =
        object ["owner" .= repoOwner, "name" .= repoName]

-- | Session state
data SessionState
    = Creating
    | Running
    | Attached
    | Stopping
    | Failed !Text
    deriving stock (Show, Eq)

instance FromJSON SessionState where
    parseJSON = withText "SessionState" $ \case
        "creating" -> pure Creating
        "running" -> pure Running
        "attached" -> pure Attached
        "stopping" -> pure Stopping
        t
            | "failed" `T.isPrefixOf` t -> pure $ Failed t
            | otherwise ->
                fail $ "Unknown state: " <> T.unpack t

-- | Agent session
data AgentSession = AgentSession
    { asId :: !Text
    , asRepo :: !Repo
    , asIssue :: !Int
    , asWorktree :: !Text
    , asTmuxName :: !Text
    , asState :: !SessionState
    , asPrompt :: !Text
    }
    deriving stock (Show, Eq)

instance FromJSON AgentSession where
    parseJSON = withObject "AgentSession" $ \o ->
        AgentSession
            <$> o .: "id"
            <*> o .: "repo"
            <*> o .: "issue"
            <*> o .: "worktree"
            <*> o .: "tmuxName"
            <*> o .: "state"
            <*> o .: "prompt"

-- | Branch sync status
data SyncStatus
    = Synced
    | Ahead !Int
    | Behind !Int
    | Diverged !Int !Int
    | LocalOnly
    deriving stock (Show, Eq)

instance FromJSON SyncStatus where
    parseJSON = withText "SyncStatus" $ \case
        "synced" -> pure Synced
        "local-only" -> pure LocalOnly
        t -> fail $ "Unknown sync: " <> T.unpack t

-- | Worktree information
data WorktreeInfo = WorktreeInfo
    { wiRepo :: !Repo
    , wiIssue :: !Int
    , wiPath :: !Text
    }
    deriving stock (Show, Eq)

instance FromJSON WorktreeInfo where
    parseJSON = withObject "WorktreeInfo" $ \o ->
        WorktreeInfo
            <$> o .: "repo"
            <*> o .: "issue"
            <*> o .: "path"

-- | Branch information
data BranchInfo = BranchInfo
    { biRepo :: !Repo
    , biIssue :: !Int
    , biName :: !Text
    , biSync :: !SyncStatus
    }
    deriving stock (Show, Eq)

instance FromJSON BranchInfo where
    parseJSON = withObject "BranchInfo" $ \o ->
        BranchInfo
            <$> o .: "repo"
            <*> o .: "issue"
            <*> o .: "name"
            <*> o .: "sync"
