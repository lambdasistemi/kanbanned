{- |
Module      : Kanbanned.UI.Kanban
Description : Kanban board rendering
-}
module Kanbanned.UI.Kanban
    ( renderKanban
    , renderHeader
    , renderSettings
    ) where

import Data.ByteString.Builder (Builder)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
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
    , Page (..)
    , currentColumnItems
    )
import Kanbanned.Terminal.ANSI
    ( Color (..)
    , Style (..)
    , clearLine
    , defaultStyle
    , drawStyledText
    , fillRect
    , moveTo
    )
import Kanbanned.UI.Layout
    ( Rect (..)
    , headerRect
    , kanbanRect
    )

-- | Render the header bar
renderHeader :: AppState -> Builder
renderHeader st =
    let rect = headerRect (stSize st)
        style =
            defaultStyle
                { styleFg = Black
                , styleBg = Cyan
                , styleBold = True
                }
        title = " kanbanned"
        projName =
            maybe
                ""
                ( \pid ->
                    case filter
                        ( \p ->
                            Kanbanned.GitHub.Types.projectId p
                                == pid
                        )
                        (stProjects st) of
                        (p : _) ->
                            " - "
                                <> Kanbanned.GitHub.Types.projectTitle
                                    p
                        [] -> ""
                )
                (cfgProjectId $ stConfig st)
        loading =
            if stLoading st then " [loading...]" else ""
        text =
            T.take (rectWidth rect) $
                title <> projName <> loading
    in  fillRect
            (rectRow rect)
            (rectCol rect)
            1
            (rectWidth rect)
            style
            <> drawStyledText
                (rectRow rect)
                (rectCol rect)
                style
                text

-- | Render the kanban board
renderKanban :: AppState -> Builder
renderKanban st =
    let hasTerminal = stTerminalActive st
        rect = kanbanRect (stSize st) hasTerminal
        items = currentColumnItems st
        selected = stSelectedIndex st
    in  renderColumnHeader st rect
            <> renderItems st rect items selected 0

-- | Render column header
renderColumnHeader :: AppState -> Rect -> Builder
renderColumnHeader st Rect{..} =
    let status = pageToStatus (stPage st)
        label =
            " "
                <> statusToText status
                <> " ("
                <> T.pack (show $ length $ currentColumnItems st)
                <> ")"
        style =
            defaultStyle
                { styleBold = True
                , styleFg = columnColor status
                , styleUnderline = True
                }
    in  drawStyledText rectRow rectCol style $
            T.take rectWidth label

-- | Render list of items
renderItems
    :: AppState
    -> Rect
    -> [ProjectItem]
    -> Int
    -> Int
    -> Builder
renderItems _ rect [] _ row
    | row < rectHeight rect - 1 =
        let emptyStyle =
                defaultStyle{styleFg = BrightBlack}
        in  drawStyledText
                (rectRow rect + row + 1)
                (rectCol rect + 1)
                emptyStyle
                "(empty)"
    | otherwise = mempty
renderItems st rect (item : rest) selected row
    | row >= rectHeight rect - 1 = mempty
    | otherwise =
        let isSelected = selected == row
            isExpanded =
                stExpandedItem st == Just (itemId item)
            itemBuild =
                renderItem
                    st
                    rect
                    item
                    isSelected
                    isExpanded
                    (row + 1)
            expandedRows =
                if isExpanded
                    then expandedHeight item rect
                    else 0
            nextRow = row + 1 + expandedRows
        in  itemBuild
                <> renderItems
                    st
                    rect
                    rest
                    selected
                    nextRow

-- | Render a single item
renderItem
    :: AppState
    -> Rect
    -> ProjectItem
    -> Bool
    -> Bool
    -> Int
    -> Builder
renderItem st Rect{..} item isSelected isExpanded row =
    let r = rectRow + row
        w = rectWidth - 2
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
        title =
            T.take
                (w - T.length number - T.length repoPrefix - 2)
                (itemTitle item)
        line =
            " "
                <> number
                <> " "
                <> repoPrefix
                <> title
        hasSession = case itemNumber item of
            Just n -> case itemRepoName item of
                Just rn ->
                    let sid = rn <> "-" <> T.pack (show n)
                    in  Map.member sid (stSessions st)
                Nothing -> False
            Nothing -> False
        sessionIndicator =
            if hasSession then " [A]" else ""
        fullLine =
            T.take w (line <> sessionIndicator)
        style
            | isSelected =
                defaultStyle
                    { styleReverse = True
                    , styleBold = True
                    }
            | otherwise = defaultStyle
    in  moveTo r rectCol
            <> clearLine
            <> drawStyledText r rectCol style fullLine
            <> if isExpanded
                then
                    renderExpanded
                        st
                        Rect
                            { rectRow = r + 1
                            , rectCol = rectCol + 2
                            , rectHeight = rectHeight
                            , rectWidth = rectWidth - 4
                            }
                        item
                else mempty

-- | Render expanded item details
renderExpanded
    :: AppState -> Rect -> ProjectItem -> Builder
renderExpanded _st Rect{..} item =
    let labelStyle =
            defaultStyle
                { styleFg = Yellow
                , styleDim = True
                }
        bodyStyle = defaultStyle{styleFg = White}
        labels = itemLabels item
        labelLine =
            if null labels
                then mempty
                else
                    drawStyledText
                        rectRow
                        rectCol
                        labelStyle
                        ( T.take rectWidth $
                            T.intercalate ", " labels
                        )
        bodyLines = case itemBody item of
            Nothing -> []
            Just b ->
                take 5 $
                    filter (not . T.null) $
                        T.lines b
        bodyBuild =
            foldMap
                ( \(i, line) ->
                    drawStyledText
                        (rectRow + i + (if null labels then 0 else 1))
                        rectCol
                        bodyStyle
                        (T.take rectWidth line)
                )
                (zip [0 ..] bodyLines)
        actionRow =
            rectRow
                + length bodyLines
                + (if null labels then 0 else 1)
        actionStyle =
            defaultStyle
                { styleFg = BrightBlack
                , styleDim = True
                }
        actions =
            "[m]ove [a]gent [o]pen [d]elete"
    in  labelLine
            <> bodyBuild
            <> drawStyledText
                actionRow
                rectCol
                actionStyle
                actions

-- | Height of expanded section
expandedHeight :: ProjectItem -> Rect -> Int
expandedHeight item _ =
    let hasLabels =
            if null (itemLabels item) then 0 else 1
        bodyLines = case itemBody item of
            Nothing -> 0
            Just b ->
                min 5 $
                    length $
                        filter (not . T.null) $
                            T.lines b
    in  hasLabels + bodyLines + 1

-- | Render settings page
renderSettings :: AppState -> Builder
renderSettings st =
    let rect = kanbanRect (stSize st) False
        cfg = stConfig st
        r = rectRow rect
        c = rectCol rect + 2
        titleStyle =
            defaultStyle
                { styleBold = True
                , styleFg = Cyan
                }
        labelStyle =
            defaultStyle{styleFg = Yellow}
        valueStyle = defaultStyle
    in  drawStyledText r c titleStyle "Settings"
            <> drawStyledText
                (r + 2)
                c
                labelStyle
                "GitHub Token: "
            <> drawStyledText
                (r + 2)
                (c + 14)
                valueStyle
                ( case cfgGitHubToken cfg of
                    Just t ->
                        T.take 8 t <> "..."
                    Nothing -> "(not set)"
                )
            <> drawStyledText
                (r + 3)
                c
                labelStyle
                "Agent Server: "
            <> drawStyledText
                (r + 3)
                (c + 14)
                valueStyle
                (cfgAgentServer cfg)
            <> drawStyledText
                (r + 4)
                c
                labelStyle
                "Project: "
            <> drawStyledText
                (r + 4)
                (c + 14)
                valueStyle
                (fromMaybe "(not selected)" (cfgProjectId cfg))
            <> drawStyledText
                (r + 6)
                c
                (defaultStyle{styleFg = BrightBlack})
                "Press 's' to cycle settings, 'q' to go back"

-- | Map page to status
pageToStatus :: Page -> KanbanStatus
pageToStatus = \case
    BacklogPage -> Backlog
    WIPPage -> WIP
    DonePage -> Done
    SettingsPage -> WIP

-- | Color for each column
columnColor :: KanbanStatus -> Color
columnColor = \case
    Backlog -> Yellow
    WIP -> Cyan
    Done -> Green
