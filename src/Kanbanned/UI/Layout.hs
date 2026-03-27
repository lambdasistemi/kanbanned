{- |
Module      : Kanbanned.UI.Layout
Description : Screen layout management
-}
module Kanbanned.UI.Layout
    ( Rect (..)
    , splitVertical
    , splitHorizontal
    , headerRect
    , contentRect
    , statusBarRect
    , terminalRect
    , kanbanRect
    ) where

import Kanbanned.Terminal.Raw (TermSize (..))

-- | A rectangle on screen (1-based coordinates)
data Rect = Rect
    { rectRow :: !Int
    , rectCol :: !Int
    , rectHeight :: !Int
    , rectWidth :: !Int
    }
    deriving stock (Show, Eq)

-- | Split a rect vertically at a given row offset
splitHorizontal :: Int -> Rect -> (Rect, Rect)
splitHorizontal h Rect{..} =
    let top =
            Rect
                { rectRow = rectRow
                , rectCol = rectCol
                , rectHeight = min h rectHeight
                , rectWidth = rectWidth
                }
        bot =
            Rect
                { rectRow = rectRow + min h rectHeight
                , rectCol = rectCol
                , rectHeight =
                    max 0 (rectHeight - min h rectHeight)
                , rectWidth = rectWidth
                }
    in  (top, bot)

-- | Split a rect horizontally at a given column offset
splitVertical :: Int -> Rect -> (Rect, Rect)
splitVertical w Rect{..} =
    let left =
            Rect
                { rectRow = rectRow
                , rectCol = rectCol
                , rectHeight = rectHeight
                , rectWidth = min w rectWidth
                }
        right =
            Rect
                { rectRow = rectRow
                , rectCol = rectCol + min w rectWidth
                , rectHeight = rectHeight
                , rectWidth =
                    max 0 (rectWidth - min w rectWidth)
                }
    in  (left, right)

-- | Header area (row 1)
headerRect :: TermSize -> Rect
headerRect TermSize{..} =
    Rect{rectRow = 1, rectCol = 1, rectHeight = 1, rectWidth = tsCols}

-- | Main content area
contentRect :: TermSize -> Bool -> Rect
contentRect TermSize{..} hasTerminal =
    let h = tsRows - 2
        w =
            if hasTerminal
                then tsCols `div` 2
                else tsCols
    in  Rect
            { rectRow = 2
            , rectCol = 1
            , rectHeight = h
            , rectWidth = w
            }

-- | Status bar (last row)
statusBarRect :: TermSize -> Rect
statusBarRect TermSize{..} =
    Rect
        { rectRow = tsRows
        , rectCol = 1
        , rectHeight = 1
        , rectWidth = tsCols
        }

-- | Terminal pane (right half when active)
terminalRect :: TermSize -> Rect
terminalRect TermSize{..} =
    let w = tsCols `div` 2
    in  Rect
            { rectRow = 2
            , rectCol = tsCols - w + 1
            , rectHeight = tsRows - 2
            , rectWidth = w
            }

-- | Kanban content area
kanbanRect :: TermSize -> Bool -> Rect
kanbanRect = contentRect
