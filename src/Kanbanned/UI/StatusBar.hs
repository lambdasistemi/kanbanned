{- |
Module      : Kanbanned.UI.StatusBar
Description : Status bar rendering
-}
module Kanbanned.UI.StatusBar
    ( renderStatusBar
    ) where

import Data.ByteString.Builder (Builder)
import Data.Text qualified as T
import Kanbanned.GitHub.Types (KanbanStatus (..), statusToText)
import Kanbanned.State
    ( AppState (..)
    , Page (..)
    , Toast (..)
    , ToastLevel (..)
    , columnCount
    )
import Kanbanned.Terminal.ANSI
    ( Color (..)
    , Style (..)
    , defaultStyle
    , drawStyledText
    , fillRect
    )
import Kanbanned.UI.Layout (Rect (..), statusBarRect)

-- | Render the status bar
renderStatusBar :: AppState -> Builder
renderStatusBar st =
    let rect = statusBarRect (stSize st)
        barStyle =
            defaultStyle
                { styleFg = White
                , styleBg = BrightBlack
                }
    in  fillRect
            (rectRow rect)
            (rectCol rect)
            1
            (rectWidth rect)
            barStyle
            <> renderColumns st rect barStyle
            <> renderHelp st rect barStyle
            <> renderToasts st rect

-- | Render column tabs with counts
renderColumns
    :: AppState -> Rect -> Style -> Builder
renderColumns st rect barStyle =
    let cols =
            [ (BacklogPage, Backlog)
            , (WIPPage, WIP)
            , (DonePage, Done)
            ]
        go (offset, acc) (page, status) =
            let count = columnCount st status
                label =
                    statusToText status
                        <> " ("
                        <> T.pack (show count)
                        <> ")"
                isActive = stPage st == page
                style =
                    if isActive
                        then
                            barStyle
                                { styleBold = True
                                , styleFg = Cyan
                                }
                        else barStyle
                sep = if offset > 0 then " | " else ""
                text = sep <> label
            in  ( offset + T.length text
                , acc
                    <> drawStyledText
                        (rectRow rect)
                        (rectCol rect + offset)
                        style
                        text
                )
    in  snd $ foldl go (0, mempty) cols

-- | Render help text on right side
renderHelp :: AppState -> Rect -> Style -> Builder
renderHelp st rect barStyle =
    let helpText = case stPage st of
            SettingsPage -> "q:quit"
            _ ->
                "h/l:col j/k:nav m:move"
                    <> " a:agent Enter:expand q:quit"
        col =
            rectCol rect
                + rectWidth rect
                - T.length helpText
    in  drawStyledText
            (rectRow rect)
            (max (rectCol rect) col)
            (barStyle{styleDim = True})
            helpText

-- | Render toast notifications above status bar
renderToasts :: AppState -> Rect -> Builder
renderToasts st rect =
    case stToasts st of
        [] -> mempty
        (Toast{..} : _) ->
            let style = case toastLevel of
                    Info ->
                        defaultStyle
                            { styleFg = Green
                            , styleBold = True
                            }
                    Error ->
                        defaultStyle
                            { styleFg = Red
                            , styleBold = True
                            }
                row = rectRow rect - 1
                msg = T.take (rectWidth rect) toastMessage
            in  drawStyledText row (rectCol rect) style msg
