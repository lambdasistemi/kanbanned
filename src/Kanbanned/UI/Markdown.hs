{- |
Module      : Kanbanned.UI.Markdown
Description : Render CommonMark markdown as brick widgets
-}
module Kanbanned.UI.Markdown
    ( renderMarkdownWidget
    ) where

import Brick (Widget, attrName, txt, vBox, withAttr, (<+>))
import CMark (Node (..), NodeType (..), commonmarkToNode)
import Data.Text (Text)
import Data.Text qualified as T
import Kanbanned.State (Name)

-- | Render markdown text as a brick widget
renderMarkdownWidget :: Text -> Widget Name
renderMarkdownWidget md =
    let children = case commonmarkToNode [] md of
            Node _ DOCUMENT cs -> cs
            _ -> []
    in  vBox $ concatMap renderBlock children

renderBlock :: Node -> [Widget Name]
renderBlock (Node _ nodeType children) = case nodeType of
    PARAGRAPH ->
        [renderInlines children, txt ""]
    HEADING level ->
        let attr = case level of
                1 -> attrName "md.h1"
                2 -> attrName "md.h2"
                _ -> attrName "md.h3"
        in  [withAttr attr $ renderInlines children, txt ""]
    CODE_BLOCK _info code ->
        map
            (\line -> withAttr (attrName "md.code") $ txt $ "  " <> line)
            (T.lines code)
            <> [txt ""]
    BLOCK_QUOTE ->
        map
            (\w -> withAttr (attrName "md.quote") $ txt "│ " <+> w)
            (concatMap renderBlock children)
    LIST _ ->
        concatMap renderListItem children
    ITEM ->
        let inlines = concatMap nodeChildren children
        in  [txt "  - " <+> renderInlines inlines]
    THEMATIC_BREAK ->
        [withAttr (attrName "md.hr") $ txt "────────────────"]
    _ -> concatMap renderBlock children

renderListItem :: Node -> [Widget Name]
renderListItem = renderBlock

renderInlines :: [Node] -> Widget Name
renderInlines nodes =
    txt $ T.concat $ map inlineToText nodes

inlineToText :: Node -> Text
inlineToText (Node _ nodeType children) = case nodeType of
    TEXT t -> t
    SOFTBREAK -> " "
    LINEBREAK -> "\n"
    CODE t -> "`" <> t <> "`"
    EMPH -> "*" <> T.concat (map inlineToText children) <> "*"
    STRONG ->
        "**" <> T.concat (map inlineToText children) <> "**"
    LINK url _title ->
        T.concat (map inlineToText children)
            <> " ("
            <> url
            <> ")"
    IMAGE url _title -> "[img: " <> url <> "]"
    _ -> T.concat $ map inlineToText children

nodeChildren :: Node -> [Node]
nodeChildren (Node _ _ cs) = cs
