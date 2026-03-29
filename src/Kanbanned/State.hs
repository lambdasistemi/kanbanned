{- |
Module      : Kanbanned.State
Description : Application state
-}
module Kanbanned.State
    ( AppState (..)
    , Page (..)
    , Name (..)
    , AppEvent (..)
    , ItemView (..)
    , TreeRow (..)
    , initialState
    , currentColumnItems
    , columnCount
    , projectItems
    , selectedItemSessionId
    , selectedItemView
    , buildTree
    ) where

import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
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

-- | What the right pane shows for an item
data ItemView = ShowDescription | ShowTerminal
    deriving stock (Show, Eq)

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
    , stCollapsed :: !(Set Text)
    -- ^ Collapsed tree nodes (org name or "org/repo")
    , stRepoFilter :: !(Set Text)
    , stLabelFilter :: !(Set Text)
    , stToast :: !(Maybe Text)
    , stTerminalFocused :: !Bool
    , stItemViews :: !(Map Text ItemView)
    -- ^ Per-session view preference
    , stTerminalImages :: !(Map Text V.Image)
    -- ^ Live terminal images keyed by session ID
    , stLoading :: !Bool
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
        , stCollapsed = mempty
        , stSessions = Map.empty
        , stBranches = Map.empty
        , stRepoFilter = mempty
        , stLabelFilter = mempty
        , stToast = Nothing
        , stTerminalFocused = False
        , stItemViews = Map.empty
        , stTerminalImages = Map.empty
        , stLoading = True
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
    filter (matchesStatus $ pageToStatus $ stPage st) $
        projectItems st

-- | Count items in a column
columnCount :: AppState -> KanbanStatus -> Int
columnCount st status =
    length $
        filter (matchesStatus status) $
            projectItems st

-- | Get the session ID for the currently selected item
selectedItemSessionId :: AppState -> Maybe Text
selectedItemSessionId st = do
    let items = currentColumnItems st
    item <- safeIdx (stSelectedIndex st) items
    repo <- itemRepoName item
    issue <- itemNumber item
    let sid = repo <> "-" <> T.pack (show issue)
    if Map.member sid (stSessions st)
        then Just sid
        else Nothing

-- | Get the view preference for the selected item
selectedItemView :: AppState -> ItemView
selectedItemView st = case selectedItemSessionId st of
    Just sid ->
        Map.findWithDefault ShowDescription sid (stItemViews st)
    Nothing -> ShowDescription

-- | A row in the flattened tree view
data TreeRow
    = -- | org name, item count, collapsed?
      OrgRow !Text !Int !Bool
    | -- | org, repo name, item count, collapsed?
      RepoRow !Text !Text !Int !Bool
    | ItemRow !ProjectItem
    deriving stock (Show)

-- | Build a flat tree from items, respecting collapsed state
buildTree :: AppState -> [TreeRow]
buildTree st =
    let items = currentColumnItems st
        collapsed = stCollapsed st
        -- Group by org
        grouped = groupByOrg items
        orgs = Map.toAscList grouped
    in  concatMap (renderOrg collapsed) orgs
  where
    groupByOrg :: [ProjectItem] -> Map Text (Map Text [ProjectItem])
    groupByOrg = foldl' addItem Map.empty
      where
        addItem acc item =
            let org = fromMaybe "other" (itemRepoOwner item)
                repo = fromMaybe "draft" (itemRepoName item)
            in  Map.insertWith
                    (Map.unionWith (<>))
                    org
                    (Map.singleton repo [item])
                    acc

    renderOrg
        :: Set Text
        -> (Text, Map Text [ProjectItem])
        -> [TreeRow]
    renderOrg collapsed (org, repos) =
        let totalItems =
                sum $ map length $ Map.elems repos
            orgCollapsed = Set.member org collapsed
        in  OrgRow org totalItems orgCollapsed
                : if orgCollapsed
                    then []
                    else
                        concatMap
                            (renderRepo collapsed org)
                            (Map.toAscList repos)

    renderRepo
        :: Set Text
        -> Text
        -> (Text, [ProjectItem])
        -> [TreeRow]
    renderRepo collapsed org (repo, repoItems) =
        let key = org <> "/" <> repo
            repoCollapsed = Set.member key collapsed
        in  RepoRow org repo (length repoItems) repoCollapsed
                : if repoCollapsed
                    then []
                    else map ItemRow repoItems

-- Internals

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

safeIdx :: Int -> [a] -> Maybe a
safeIdx _ [] = Nothing
safeIdx 0 (x : _) = Just x
safeIdx n (_ : xs)
    | n > 0 = safeIdx (n - 1) xs
    | otherwise = Nothing
