{- |
Module      : Kanbanned.UI.Draw
Description : Brick UI drawing
-}
module Kanbanned.UI.Draw
    ( drawUI
    ) where

import Brick
    ( AttrName
    , Padding (..)
    , Widget
    , attrName
    , emptyWidget
    , hBox
    , padBottom
    , padLeft
    , padRight
    , raw
    , str
    , txt
    , vBox
    , withAttr
    , (<+>)
    , (<=>)
    )
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Kanbanned.Config (Config (..))
import Kanbanned.GitHub.Types
    ( ItemType (..)
    , KanbanStatus (..)
    , Project (..)
    , ProjectItem (..)
    , statusToText
    )
import Kanbanned.State
    ( AppState (..)
    , Name (..)
    , Page (..)
    , columnCount
    , currentColumnItems
    )
import Kanbanned.UI.Markdown (renderMarkdownWidget)

-- | Main draw function for brick
drawUI :: AppState -> [Widget Name]
drawUI st = [ui]
  where
    ui =
        drawHeader st
            <=> padBottom Max (drawBody st)
            <=> drawToast st
            <=> drawStatusBar st

drawBody :: AppState -> Widget Name
drawBody st
    | stTerminalActive st
    , Just img <- stTerminalImage st =
        drawContent st <+> drawTerminalPane st img
    | otherwise = drawContent st

drawTerminalPane :: AppState -> V.Image -> Widget Name
drawTerminalPane st img =
    let sid =
            fromMaybe "?" (stActiveTerminal st)
        header =
            withAttr (attrName "header") $
                padRight Max $
                    txt (" Terminal: " <> sid)
    in  header <=> raw img

------------------------------------------------------------------------
-- Header
------------------------------------------------------------------------

drawHeader :: AppState -> Widget Name
drawHeader st =
    withAttr (attrName "header") $
        padRight Max $
            txt $
                " kanbanned"
                    <> projName
                    <> loading
  where
    projName = case cfgProjectId (stConfig st) of
        Just pid ->
            case filter (\p -> projectId p == pid) (stProjects st) of
                (p : _) -> " - " <> projectTitle p
                [] -> ""
        Nothing -> ""
    loading =
        if stLoading st then " [loading...]" else ""

------------------------------------------------------------------------
-- Content
------------------------------------------------------------------------

drawContent :: AppState -> Widget Name
drawContent st = case stPage st of
    SettingsPage -> drawSettings st
    _ -> drawKanban st

drawKanban :: AppState -> Widget Name
drawKanban st =
    drawColumnHeader st
        <=> drawItems st

drawColumnHeader :: AppState -> Widget Name
drawColumnHeader st =
    let status = pageToStatus (stPage st)
        label =
            " "
                <> statusToText status
                <> " ("
                <> T.pack (show $ length $ currentColumnItems st)
                <> ")"
    in  withAttr (columnAttr status) $ padRight Max $ txt label

drawItems :: AppState -> Widget Name
drawItems st =
    let items = currentColumnItems st
    in  if null items
            then withAttr (attrName "dim") $ str "(empty)"
            else vBox $ zipWith (drawItem st) [0 ..] items

drawItem :: AppState -> Int -> ProjectItem -> Widget Name
drawItem st idx item =
    let isSelected = stSelectedIndex st == idx
        isExpanded = stExpandedItem st == Just (itemId item)
        prefix = case itemType item of
            IssueItem -> "#"
            PullRequestItem -> "PR#"
            DraftIssueItem -> "*"
        number = case itemNumber item of
            Just n -> prefix <> T.pack (show n)
            Nothing -> prefix <> "draft"
        repo = fromMaybe "" (itemRepoName item)
        repoPrefix =
            if T.null repo then "" else repo <> " "
        hasSession = case itemNumber item of
            Just n -> case itemRepoName item of
                Just rn ->
                    let sid = rn <> "-" <> T.pack (show n)
                    in  Map.member sid (stSessions st)
                Nothing -> False
            Nothing -> False
        indicator = if hasSession then " [A]" else ""
        line =
            " "
                <> number
                <> " "
                <> repoPrefix
                <> itemTitle item
                <> indicator
        attr =
            if isSelected
                then attrName "selected"
                else attrName "item"
        itemLine = withAttr attr $ padRight Max $ txt line
        expanded =
            if isExpanded
                then drawExpanded item
                else emptyWidget
    in  itemLine <=> expanded

drawExpanded :: ProjectItem -> Widget Name
drawExpanded item =
    let labels = itemLabels item
        labelLine =
            if null labels
                then emptyWidget
                else
                    withAttr (attrName "label") $
                        padLeft (Pad 2) $
                            txt $
                                T.intercalate ", " labels
        bodyWidget = case itemBody item of
            Nothing -> emptyWidget
            Just b ->
                padLeft (Pad 2) $ renderMarkdownWidget b
        actions =
            withAttr (attrName "dim") $
                padLeft (Pad 2) $
                    txt "[m]ove [a]gent [o]pen [d]elete"
    in  labelLine <=> bodyWidget <=> actions

drawSettings :: AppState -> Widget Name
drawSettings st =
    let cfg = stConfig st
    in  vBox
            [ withAttr (attrName "header") $ txt "Settings"
            , str ""
            , hBox
                [ withAttr (attrName "label") $ txt "GitHub Token: "
                , txt $ case cfgGitHubToken cfg of
                    Just t -> T.take 8 t <> "..."
                    Nothing -> "(not set)"
                ]
            , hBox
                [ withAttr (attrName "label") $ txt "Agent Server: "
                , txt $ cfgAgentServer cfg
                ]
            , hBox
                [ withAttr (attrName "label") $ txt "Project: "
                , txt $
                    fromMaybe "(not selected)" (cfgProjectId cfg)
                ]
            , str ""
            , withAttr (attrName "dim") $
                txt "Press 's' to cycle settings, 'q' to go back"
            ]

------------------------------------------------------------------------
-- Status bar
------------------------------------------------------------------------

drawStatusBar :: AppState -> Widget Name
drawStatusBar st =
    withAttr (attrName "statusbar") $
        padRight Max $
            hBox
                [ drawTabs st
                , padLeft Max $ drawHelp st
                ]

drawTabs :: AppState -> Widget Name
drawTabs st =
    hBox $
        zipWith
            drawTab
            [BacklogPage, WIPPage, DonePage]
            [Backlog, WIP, Done]
  where
    drawTab page status =
        let count = columnCount st status
            label =
                statusToText status
                    <> " ("
                    <> T.pack (show count)
                    <> ")"
            attr =
                if stPage st == page
                    then attrName "tab.active"
                    else attrName "tab"
        in  withAttr attr (txt $ " " <> label <> " ")

drawHelp :: AppState -> Widget Name
drawHelp st =
    withAttr (attrName "dim") $ txt $ case stPage st of
        SettingsPage -> "q:quit"
        _ -> "h/l:col j/k:nav m:move a:agent Enter:expand q:quit"

drawToast :: AppState -> Widget Name
drawToast st = case stToast st of
    Nothing -> emptyWidget
    Just msg -> withAttr (attrName "toast") $ txt msg

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

pageToStatus :: Page -> KanbanStatus
pageToStatus = \case
    BacklogPage -> Backlog
    WIPPage -> WIP
    DonePage -> Done
    SettingsPage -> WIP

columnAttr :: KanbanStatus -> AttrName
columnAttr = \case
    Backlog -> attrName "column.backlog"
    WIP -> attrName "column.wip"
    Done -> attrName "column.done"
