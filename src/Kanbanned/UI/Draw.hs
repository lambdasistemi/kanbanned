{- |
Module      : Kanbanned.UI.Draw
Description : Brick UI drawing — split pane layout
-}
module Kanbanned.UI.Draw
    ( drawUI
    ) where

import Brick
    ( AttrName
    , Padding (..)
    , Widget
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
import Brick.Widgets.Border (vBorder)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Kanbanned.App.Attrs
    ( columnBacklogAttr
    , columnDoneAttr
    , columnWipAttr
    , dimAttr
    , headerAttr
    , itemAttr
    , labelAttr
    , selectedAttr
    , statusbarAttr
    , tabActiveAttr
    , tabAttr
    , toastAttr
    )
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
    , ItemView (..)
    , Name (..)
    , Page (..)
    , columnCount
    , currentColumnItems
    )
import Kanbanned.UI.Markdown (renderMarkdownWidget)

-- | Main draw function
drawUI :: AppState -> [Widget Name]
drawUI st = [ui]
  where
    ui =
        drawHeader st
            <=> padBottom Max (drawBody st)
            <=> drawToast st
            <=> drawStatusBar st

------------------------------------------------------------------------
-- Body — split pane: item list | detail/terminal
------------------------------------------------------------------------

drawBody :: AppState -> Widget Name
drawBody st = case stPage st of
    SettingsPage -> drawSettings st
    _ ->
        let items = currentColumnItems st
            leftPane =
                drawColumnHeader st
                    <=> drawItemList st items
            rightPane = drawDetailPane st items
        in  leftPane <+> vBorder <+> rightPane

------------------------------------------------------------------------
-- Left pane — item list (always flat)
------------------------------------------------------------------------

drawColumnHeader :: AppState -> Widget Name
drawColumnHeader st =
    let status = pageToStatus (stPage st)
        label =
            " "
                <> statusToText status
                <> " ("
                <> T.pack
                    (show $ length $ currentColumnItems st)
                <> ")"
    in  withAttr (columnAttr status) $
            padRight Max $
                txt label

drawItemList :: AppState -> [ProjectItem] -> Widget Name
drawItemList st items
    | null items = withAttr dimAttr $ str "(empty)"
    | otherwise =
        vBox $ zipWith (drawItemRow st) [0 ..] items

drawItemRow
    :: AppState -> Int -> ProjectItem -> Widget Name
drawItemRow st idx item =
    let isSelected = stSelectedIndex st == idx
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
                    let sid =
                            rn <> "-" <> T.pack (show n)
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
                then selectedAttr
                else itemAttr
    in  withAttr attr $ padRight Max $ txt line

------------------------------------------------------------------------
-- Right pane — issue detail or terminal
------------------------------------------------------------------------

drawDetailPane
    :: AppState -> [ProjectItem] -> Widget Name
drawDetailPane st items = case selectedItem st items of
    Nothing ->
        withAttr dimAttr $
            padLeft (Pad 1) $
                txt "Select an item"
    Just item ->
        let mSid = itemSid item st
            view = case mSid of
                Just sid ->
                    Map.findWithDefault
                        ShowDescription
                        sid
                        (stItemViews st)
                Nothing -> ShowDescription
        in  case (view, mSid) of
                (ShowTerminal, Just sid)
                    | Just img <-
                        Map.lookup sid (stTerminalImages st) ->
                        drawTerminalView sid img
                _ -> drawItemDetail item

drawTerminalView :: T.Text -> V.Image -> Widget Name
drawTerminalView sid img =
    let header =
            withAttr headerAttr $
                padRight Max $
                    txt (" Terminal: " <> sid)
    in  header <=> raw img

-- | Get session ID for an item
itemSid :: ProjectItem -> AppState -> Maybe T.Text
itemSid item st = do
    repo <- itemRepoName item
    issue <- itemNumber item
    let sid = repo <> "-" <> T.pack (show issue)
    if Map.member sid (stSessions st)
        then Just sid
        else Nothing

drawItemDetail :: ProjectItem -> Widget Name
drawItemDetail item =
    padLeft (Pad 1) $
        vBox
            [ -- Title
              withAttr headerAttr $
                padRight Max $
                    txt (" " <> itemTitle item)
            , -- Labels
              if null (itemLabels item)
                then emptyWidget
                else
                    withAttr labelAttr $
                        txt $
                            T.intercalate ", " (itemLabels item)
            , str ""
            , -- Body (markdown)
              case itemBody item of
                Nothing ->
                    withAttr dimAttr $ txt "(no description)"
                Just b -> renderMarkdownWidget b
            , str ""
            , -- Actions
              withAttr dimAttr $
                txt "[m]ove [a]gent [Tab]terminal [q]uit"
            ]

selectedItem :: AppState -> [ProjectItem] -> Maybe ProjectItem
selectedItem st items
    | stSelectedIndex st < length items =
        Just (items !! stSelectedIndex st)
    | otherwise = Nothing

------------------------------------------------------------------------
-- Settings
------------------------------------------------------------------------

drawSettings :: AppState -> Widget Name
drawSettings st =
    let cfg = stConfig st
    in  vBox
            [ withAttr headerAttr $ txt "Settings"
            , str ""
            , hBox
                [ withAttr labelAttr $
                    txt "GitHub Token: "
                , txt $ case cfgGitHubToken cfg of
                    Just t -> T.take 8 t <> "..."
                    Nothing -> "(not set)"
                ]
            , hBox
                [ withAttr labelAttr $
                    txt "Agent Server: "
                , txt $ cfgAgentServer cfg
                ]
            , hBox
                [ withAttr labelAttr $ txt "Project: "
                , txt $
                    fromMaybe
                        "(not selected)"
                        (cfgProjectId cfg)
                ]
            , str ""
            , withAttr dimAttr $
                txt "Press 'q' to go back"
            ]

------------------------------------------------------------------------
-- Header
------------------------------------------------------------------------

drawHeader :: AppState -> Widget Name
drawHeader st =
    withAttr headerAttr $
        padRight Max $
            txt $
                " kanbanned" <> projName <> loading
  where
    projName = case cfgProjectId (stConfig st) of
        Just pid ->
            case filter
                (\p -> projectId p == pid)
                (stProjects st) of
                (p : _) -> " - " <> projectTitle p
                [] -> ""
        Nothing -> ""
    loading =
        if stLoading st
            then " [loading...]"
            else ""

------------------------------------------------------------------------
-- Status bar
------------------------------------------------------------------------

drawStatusBar :: AppState -> Widget Name
drawStatusBar st =
    withAttr statusbarAttr $
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
                    then tabActiveAttr
                    else tabAttr
        in  withAttr attr (txt $ " " <> label <> " ")

drawHelp :: AppState -> Widget Name
drawHelp _st =
    withAttr dimAttr $
        txt
            "h/l:col j/k:nav Enter:detail m:move a:agent Tab:term q:quit"

drawToast :: AppState -> Widget Name
drawToast st = case stToast st of
    Nothing -> emptyWidget
    Just msg -> withAttr toastAttr $ txt msg

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
    Backlog -> columnBacklogAttr
    WIP -> columnWipAttr
    Done -> columnDoneAttr
