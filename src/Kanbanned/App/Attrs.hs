{- |
Module      : Kanbanned.App.Attrs
Description : Attribute map for the TUI
-}
module Kanbanned.App.Attrs
    ( theAttrMap
    , headerAttr
    , selectedAttr
    , itemAttr
    , dimAttr
    , labelAttr
    , columnBacklogAttr
    , columnWipAttr
    , columnDoneAttr
    , statusbarAttr
    , tabActiveAttr
    , tabAttr
    , toastAttr
    , mdH1Attr
    , mdH2Attr
    , mdH3Attr
    , mdCodeAttr
    , mdQuoteAttr
    , mdHrAttr
    ) where

import Brick (AttrMap, AttrName, attrMap, attrName, on)
import Graphics.Vty qualified as V

-- | Header bar attribute
headerAttr :: AttrName
headerAttr = attrName "header"

-- | Selected item attribute
selectedAttr :: AttrName
selectedAttr = attrName "selected"

-- | Normal item attribute
itemAttr :: AttrName
itemAttr = attrName "item"

-- | Dimmed text attribute
dimAttr :: AttrName
dimAttr = attrName "dim"

-- | Label attribute
labelAttr :: AttrName
labelAttr = attrName "label"

-- | Backlog column header attribute
columnBacklogAttr :: AttrName
columnBacklogAttr = attrName "column.backlog"

-- | WIP column header attribute
columnWipAttr :: AttrName
columnWipAttr = attrName "column.wip"

-- | Done column header attribute
columnDoneAttr :: AttrName
columnDoneAttr = attrName "column.done"

-- | Status bar attribute
statusbarAttr :: AttrName
statusbarAttr = attrName "statusbar"

-- | Active tab attribute
tabActiveAttr :: AttrName
tabActiveAttr = attrName "tab.active"

-- | Inactive tab attribute
tabAttr :: AttrName
tabAttr = attrName "tab"

-- | Toast notification attribute
toastAttr :: AttrName
toastAttr = attrName "toast"

-- | Markdown heading 1 attribute
mdH1Attr :: AttrName
mdH1Attr = attrName "md.h1"

-- | Markdown heading 2 attribute
mdH2Attr :: AttrName
mdH2Attr = attrName "md.h2"

-- | Markdown heading 3 attribute
mdH3Attr :: AttrName
mdH3Attr = attrName "md.h3"

-- | Markdown code attribute
mdCodeAttr :: AttrName
mdCodeAttr = attrName "md.code"

-- | Markdown block quote attribute
mdQuoteAttr :: AttrName
mdQuoteAttr = attrName "md.quote"

-- | Markdown horizontal rule attribute
mdHrAttr :: AttrName
mdHrAttr = attrName "md.hr"

-- | Application attribute map
theAttrMap :: AttrMap
theAttrMap =
    attrMap
        V.defAttr
        [ (headerAttr, V.black `on` V.cyan)
        , (selectedAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (itemAttr, V.defAttr)
        , (dimAttr, fg V.brightBlack)
        , (labelAttr, fg V.yellow)
        , (columnBacklogAttr, fg V.yellow `V.withStyle` V.bold)
        , (columnWipAttr, fg V.cyan `V.withStyle` V.bold)
        , (columnDoneAttr, fg V.green `V.withStyle` V.bold)
        , (statusbarAttr, V.white `on` V.brightBlack)
        , (tabActiveAttr, fg V.cyan `V.withStyle` V.bold)
        , (tabAttr, fg V.white)
        , (toastAttr, fg V.green `V.withStyle` V.bold)
        , (mdH1Attr, fg V.cyan `V.withStyle` V.bold)
        , (mdH2Attr, fg V.yellow `V.withStyle` V.bold)
        , (mdH3Attr, fg V.magenta `V.withStyle` V.bold)
        , (mdCodeAttr, fg V.green)
        , (mdQuoteAttr, fg V.brightBlack `V.withStyle` V.italic)
        , (mdHrAttr, fg V.brightBlack)
        ]
  where
    fg = V.withForeColor V.defAttr
