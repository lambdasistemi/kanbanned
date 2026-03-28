{- |
Module      : Kanbanned.UI.Terminal
Description : Embedded terminal view via libvterm as a brick widget
-}
module Kanbanned.UI.Terminal
    ( TerminalView (..)
    , newTerminalView
    , feedTerminalView
    , renderTerminalWidget
    , getTerminalImage
    , freeTerminalView
    ) where

import Brick (Widget, raw)
import Data.ByteString (ByteString)
import Data.Text qualified as T
import Data.Word (Word8)
import Graphics.Vty qualified as V
import Kanbanned.State (Name)
import System.Terminal.LibVTerm
    ( Term
    , feedInput
    , freeTerm
    , getGrid
    , newTerm
    )
import System.Terminal.LibVTerm.Types
    ( Cell (..)
    , CellAttrs (..)
    )
import System.Terminal.LibVTerm.Types qualified as VT

-- | Embedded terminal view state
data TerminalView = TerminalView
    { tvTerm :: !Term
    , tvSessionId :: !(Maybe T.Text)
    }

-- | Create a new terminal view
newTerminalView
    :: Int -> Int -> Maybe T.Text -> IO TerminalView
newTerminalView rows cols sessionId = do
    term <- newTerm rows cols
    pure TerminalView{tvTerm = term, tvSessionId = sessionId}

-- | Feed bytes into the terminal emulator
feedTerminalView :: TerminalView -> ByteString -> IO ()
feedTerminalView TerminalView{..} = feedInput tvTerm

-- | Free terminal resources
freeTerminalView :: TerminalView -> IO ()
freeTerminalView TerminalView{..} = freeTerm tvTerm

-- | Render the terminal as a brick widget (IO)
renderTerminalWidget :: TerminalView -> IO (Widget Name)
renderTerminalWidget tv = do
    image <- getTerminalImage tv
    pure $ raw image

-- | Get the terminal content as a vty Image (IO)
getTerminalImage :: TerminalView -> IO V.Image
getTerminalImage TerminalView{..} = do
    grid <- getGrid tvTerm
    pure $ V.vertCat $ map renderRow grid

renderRow :: [Cell] -> V.Image
renderRow = V.horizCat . map renderSpan . coalesce
  where
    -- Group consecutive cells with same attributes
    coalesce :: [Cell] -> [(V.Attr, T.Text)]
    coalesce [] = []
    coalesce (c : cs) =
        let attr = cellAttr c
            txt = cellChar c
            (same, rest) = span (\c' -> cellAttr c' == attr) cs
            combined = txt <> T.concat (map cellChar same)
        in  (attr, combined) : coalesce rest

    cellAttr Cell{..} = cellToVtyAttr cellAttrs cellFg cellBg
    cellChar Cell{..} =
        if T.null cellChars then " " else cellChars

renderSpan :: (V.Attr, T.Text) -> V.Image
renderSpan (attr, txt) = V.text' attr txt

cellToVtyAttr :: CellAttrs -> VT.Color -> VT.Color -> V.Attr
cellToVtyAttr CellAttrs{..} fg bg =
    let base =
            applyFg fg (applyBg bg V.defAttr)
        applyFg VT.ColorDefault a = a
        applyFg c a = V.withForeColor a (vtColorToVty c)
        applyBg VT.ColorDefault a = a
        applyBg c a = V.withBackColor a (vtColorToVty c)
        withBold a = if attrBold then a `V.withStyle` V.bold else a
        withItalic a = if attrItalic then a `V.withStyle` V.italic else a
        withUl a = if attrUnderline > 0 then a `V.withStyle` V.underline else a
        withRev a = if attrReverse then a `V.withStyle` V.reverseVideo else a
    in  withRev $ withUl $ withItalic $ withBold base

vtColorToVty :: VT.Color -> V.Color
vtColorToVty = \case
    VT.ColorDefault -> V.white
    VT.ColorRGB r g b -> V.rgbColor r g b
    VT.ColorIndex i
        -- In FullColor mode, convert indexed colors to RGB
        -- for maximum fidelity
        | i < 16 -> V.ISOColor (fromIntegral i)
        | i < 232 ->
            -- 216-color cube: 16 + 36*r + 6*g + b
            let idx = fromIntegral i - 16 :: Int
                r = idx `div` 36
                g = (idx `mod` 36) `div` 6
                b = idx `mod` 6
                toRGB :: Int -> Word8
                toRGB n =
                    if n == 0
                        then 0
                        else fromIntegral (55 + 40 * n)
            in  V.rgbColor (toRGB r) (toRGB g) (toRGB b)
        | otherwise ->
            -- Grayscale: 232-255 → 8, 18, ..., 238
            let g = 8 + 10 * fromIntegral (i - 232) :: Word8
            in  V.rgbColor g g g
