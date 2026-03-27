{- |
Module      : Kanbanned.State
Description : Application state management
-}
module Kanbanned.State
    ( AppState (..)
    , Page (..)
    , Toast (..)
    , ToastLevel (..)
    , initialState
    , currentColumnItems
    , columnCount
    ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Text (Text)
import Kanbanned.Agent.Types
    ( AgentSession
    , BranchInfo
    )
import Kanbanned.Config (Config (..))
import Kanbanned.GitHub.Types
    ( KanbanStatus (..)
    , Project
    , ProjectItem (..)
    , StatusField
    )
import Kanbanned.Terminal.Raw (TermSize (..))

-- | Current page/view
data Page
    = BacklogPage
    | WIPPage
    | DonePage
    | SettingsPage
    deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Toast notification level
data ToastLevel = Info | Error
    deriving stock (Show, Eq)

-- | Toast notification
data Toast = Toast
    { toastMessage :: !Text
    , toastLevel :: !ToastLevel
    , toastTTL :: !Int
    }
    deriving stock (Show, Eq)

-- | Application state
data AppState = AppState
    { stConfig :: !Config
    -- ^ Current config
    , stSize :: !TermSize
    -- ^ Terminal size
    , stPage :: !Page
    -- ^ Current page
    , stProjects :: ![Project]
    -- ^ Available projects
    , stItems :: !(Map Text [ProjectItem])
    -- ^ Items per project (by project ID)
    , stStatusFields :: !(Map Text StatusField)
    -- ^ Status field per project
    , stSelectedIndex :: !Int
    -- ^ Currently selected item index
    , stExpandedItem :: !(Maybe Text)
    -- ^ Expanded item ID
    , stSessions :: !(Map Text AgentSession)
    -- ^ Active agent sessions
    , stBranches :: !(Map Text BranchInfo)
    -- ^ Known branches
    , stRepoFilter :: !(Set Text)
    -- ^ Repo filter
    , stLabelFilter :: !(Set Text)
    -- ^ Label filter
    , stToasts :: ![Toast]
    -- ^ Active toasts
    , stInputMode :: !(Maybe InputMode)
    -- ^ Input mode (for text input)
    , stTerminalActive :: !Bool
    -- ^ Whether terminal pane is active
    , stActiveTerminal :: !(Maybe Text)
    -- ^ Active terminal session ID
    , stLoading :: !Bool
    -- ^ Loading indicator
    }
    deriving stock (Show)

-- | Input mode for text entry
data InputMode
    = NewItemInput !Text
    | FilterInput !Text
    deriving stock (Show, Eq)

-- | Create initial application state
initialState :: Config -> TermSize -> AppState
initialState cfg size =
    AppState
        { stConfig = cfg
        , stSize = size
        , stPage = WIPPage
        , stProjects = []
        , stItems = Map.empty
        , stStatusFields = Map.empty
        , stSelectedIndex = 0
        , stExpandedItem = Nothing
        , stSessions = Map.empty
        , stBranches = Map.empty
        , stRepoFilter = mempty
        , stLabelFilter = mempty
        , stToasts = []
        , stInputMode = Nothing
        , stTerminalActive = False
        , stActiveTerminal = Nothing
        , stLoading = True
        }

-- | Get items for current column
currentColumnItems :: AppState -> [ProjectItem]
currentColumnItems st =
    let status = pageToStatus (stPage st)
        projId = cfgProjectId (stConfig st)
        allItems = case projId of
            Just pid ->
                Map.findWithDefault [] pid (stItems st)
            Nothing -> []
    in  filter (matchesStatus status) $
            filter (matchesFilters st) allItems

-- | Count items in a column
columnCount :: AppState -> KanbanStatus -> Int
columnCount st status =
    let projId = cfgProjectId (stConfig st)
        allItems = case projId of
            Just pid ->
                Map.findWithDefault [] pid (stItems st)
            Nothing -> []
    in  length $
            filter (matchesStatus status) $
                filter (matchesFilters st) allItems

-- | Map page to kanban status
pageToStatus :: Page -> KanbanStatus
pageToStatus = \case
    BacklogPage -> Backlog
    WIPPage -> WIP
    DonePage -> Done
    SettingsPage -> WIP

-- | Check if item matches current status
matchesStatus :: KanbanStatus -> ProjectItem -> Bool
matchesStatus status item =
    itemStatus item == Just status

-- | Check if item matches active filters
matchesFilters :: AppState -> ProjectItem -> Bool
matchesFilters st item =
    matchesRepo && matchesLabel
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
