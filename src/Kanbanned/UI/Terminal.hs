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
renderRow cells = V.horizCat $ map renderCell cells

renderCell :: Cell -> V.Image
renderCell Cell{..} =
    let attr = cellToVtyAttr cellAttrs cellFg cellBg
        ch =
            if T.null cellChars
                then " "
                else cellChars
    in  V.text' attr ch

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
        | i < 16 -> V.ISOColor (fromIntegral i)
        | otherwise -> V.Color240 (fromIntegral i - 16)
