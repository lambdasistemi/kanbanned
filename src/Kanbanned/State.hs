{- |
Module      : Kanbanned.State
Description : Application state
-}
module Kanbanned.State
    ( AppState (..)
    , Page (..)
    , Name (..)
    , AppEvent (..)
    , initialState
    , currentColumnItems
    , columnCount
    , projectItems
    ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Graphics.Vty qualified as V
import Kanbanned.Agent.Types (AgentSession, BranchInfo)
import Kanbanned.Config (Config (..))
import Kanbanned.GitHub.Types
    ( KanbanStatus (..)
    , Project
    , ProjectItem (..)
    , StatusField
    )

-- | Current page/view
data Page
    = BacklogPage
    | WIPPage
    | DonePage
    | SettingsPage
    deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Brick resource names
data Name
    = ItemList
    | TerminalPane
    deriving stock (Show, Eq, Ord)

-- | Custom events pushed from async threads
data AppEvent
    = StateUpdate !(AppState -> AppState)
    | ToastEvent !Text
    | ErrorEvent !Text

instance Show AppEvent where
    show (StateUpdate _) = "StateUpdate"
    show (ToastEvent t) = "ToastEvent " <> show t
    show (ErrorEvent t) = "ErrorEvent " <> show t

-- | Application state
data AppState = AppState
    { stConfig :: !Config
    , stPage :: !Page
    , stProjects :: ![Project]
    , stItems :: !(Map Text [ProjectItem])
    , stStatusFields :: !(Map Text StatusField)
    , stSelectedIndex :: !Int
    , stSessions :: !(Map Text AgentSession)
    , stBranches :: !(Map Text BranchInfo)
    , stRepoFilter :: !(Set Text)
    , stLabelFilter :: !(Set Text)
    , stToast :: !(Maybe Text)
    , stTerminalActive :: !Bool
    , stActiveTerminal :: !(Maybe Text)
    , stLoading :: !Bool
    , stTerminalImage :: !(Maybe V.Image)
    }

-- | Create initial application state
initialState :: Config -> AppState
initialState cfg =
    AppState
        { stConfig = cfg
        , stPage = WIPPage
        , stProjects = []
        , stItems = Map.empty
        , stStatusFields = Map.empty
        , stSelectedIndex = 0
        , stSessions = Map.empty
        , stBranches = Map.empty
        , stRepoFilter = mempty
        , stLabelFilter = mempty
        , stToast = Nothing
        , stTerminalActive = False
        , stActiveTerminal = Nothing
        , stLoading = True
        , stTerminalImage = Nothing
        }

-- | Get all filtered items for the configured project
projectItems :: AppState -> [ProjectItem]
projectItems st = case cfgProjectId (stConfig st) of
    Just pid ->
        filter (matchesFilters st) $
            Map.findWithDefault [] pid (stItems st)
    Nothing -> []

-- | Get items for current column
currentColumnItems :: AppState -> [ProjectItem]
currentColumnItems st =
    let status = pageToStatus (stPage st)
    in  filter (matchesStatus status) $ projectItems st

-- | Count items in a column
columnCount :: AppState -> KanbanStatus -> Int
columnCount st status =
    length $ filter (matchesStatus status) $ projectItems st

pageToStatus :: Page -> KanbanStatus
pageToStatus = \case
    BacklogPage -> Backlog
    WIPPage -> WIP
    DonePage -> Done
    SettingsPage -> WIP

matchesStatus :: KanbanStatus -> ProjectItem -> Bool
matchesStatus status item = itemStatus item == Just status

matchesFilters :: AppState -> ProjectItem -> Bool
matchesFilters st item = matchesRepo && matchesLabel
  where
    matchesRepo
        | null (stRepoFilter st) = True
        | otherwise = case itemRepoName item of
            Just rn -> rn `elem` stRepoFilter st
            Nothing -> True
    matchesLabel
        | null (stLabelFilter st) = True
        | otherwise =
            any (`elem` stLabelFilter st) (itemLabels item)
