{- |
Module      : Kanbanned.App.Attrs
Description : Attribute map for the TUI
-}
module Kanbanned.App.Attrs
    ( theAttrMap
    ) where

import Brick (AttrMap, attrMap, attrName, on)
import Graphics.Vty qualified as V

-- | Application attribute map
theAttrMap :: AttrMap
theAttrMap =
    attrMap
        V.defAttr
        [ (an "header", V.black `on` V.cyan)
        , (an "selected", V.defAttr `V.withStyle` V.reverseVideo)
        , (an "item", V.defAttr)
        , (an "dim", fg V.brightBlack)
        , (an "label", fg V.yellow)
        , (an "column.backlog", fg V.yellow `V.withStyle` V.bold)
        , (an "column.wip", fg V.cyan `V.withStyle` V.bold)
        , (an "column.done", fg V.green `V.withStyle` V.bold)
        , (an "statusbar", V.white `on` V.brightBlack)
        , (an "tab.active", fg V.cyan `V.withStyle` V.bold)
        , (an "tab", fg V.white)
        , (an "toast", fg V.green `V.withStyle` V.bold)
        , (an "md.h1", fg V.cyan `V.withStyle` V.bold)
        , (an "md.h2", fg V.yellow `V.withStyle` V.bold)
        , (an "md.h3", fg V.magenta `V.withStyle` V.bold)
        , (an "md.code", fg V.green)
        ,
            ( an "md.quote"
            , fg V.brightBlack `V.withStyle` V.italic
            )
        , (an "md.hr", fg V.brightBlack)
        ]
  where
    an = attrName
    fg = V.withForeColor V.defAttr
