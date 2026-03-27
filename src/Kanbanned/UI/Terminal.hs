{- |
Module      : Kanbanned.UI.Terminal
Description : Embedded terminal view via libvterm
-}
module Kanbanned.UI.Terminal
    ( TerminalView (..)
    , newTerminalView
    , feedTerminalView
    , renderTerminalView
    , resizeTerminalView
    , freeTerminalView
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Kanbanned.Terminal.ANSI
    ( Color (..)
    , Style (..)
    , defaultStyle
    , drawStyledText
    , fillRect
    , moveTo
    , resetStyle
    , withStyle
    )
import Kanbanned.UI.Layout (Rect (..))
import System.Terminal.LibVTerm
    ( Term
    , feedInput
    , freeTerm
    , getGrid
    , newTerm
    , resizeTerm
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
    pure
        TerminalView
            { tvTerm = term
            , tvSessionId = sessionId
            }

-- | Feed bytes into the terminal emulator
feedTerminalView :: TerminalView -> ByteString -> IO ()
feedTerminalView TerminalView{..} = feedInput tvTerm

-- | Resize the terminal emulator
resizeTerminalView
    :: TerminalView -> Int -> Int -> IO TerminalView
resizeTerminalView tv rows cols = do
    term' <- resizeTerm (tvTerm tv) rows cols
    pure tv{tvTerm = term'}

-- | Free terminal resources
freeTerminalView :: TerminalView -> IO ()
freeTerminalView TerminalView{..} = freeTerm tvTerm

-- | Render the terminal view into a screen region
renderTerminalView :: TerminalView -> Rect -> IO Builder
renderTerminalView TerminalView{..} rect = do
    grid <- getGrid tvTerm
    let rows = take (rectHeight rect) grid
        headerStyle =
            defaultStyle
                { styleFg = Black
                , styleBg = Magenta
                , styleBold = True
                }
        header =
            case tvSessionId of
                Just sid ->
                    drawStyledText
                        (rectRow rect)
                        (rectCol rect)
                        headerStyle
                        ( T.take (rectWidth rect) $
                            " Terminal: " <> sid <> " "
                        )
                Nothing -> mempty
        border =
            fillRect
                (rectRow rect)
                (rectCol rect)
                (rectHeight rect)
                1
                defaultStyle{styleFg = BrightBlack}
    pure $
        border
            <> header
            <> foldMap
                ( \(rowIdx, cells) ->
                    renderRow
                        rect
                        (rowIdx + 1)
                        cells
                )
                (zip [0 ..] rows)

-- | Render a single row of cells
renderRow :: Rect -> Int -> [Cell] -> Builder
renderRow Rect{..} rowOffset cells =
    let screenRow = rectRow + rowOffset
        visibleCells = take (rectWidth - 1) cells
    in  moveTo screenRow (rectCol + 1)
            <> foldMap renderCell visibleCells
            <> resetStyle

-- | Render a single cell with its attributes
renderCell :: Cell -> Builder
renderCell Cell{..} =
    let style = cellToStyle cellAttrs cellFg cellBg
    in  withStyle style
            <> if T.null cellChars
                then B.char7 ' '
                else B.byteString (TE.encodeUtf8 cellChars)

-- | Convert libvterm cell attributes to our Style
cellToStyle :: CellAttrs -> VT.Color -> VT.Color -> Style
cellToStyle CellAttrs{..} fg bg =
    Style
        { styleFg = vtColorToColor fg
        , styleBg = vtColorToColor bg
        , styleBold = attrBold
        , styleItalic = attrItalic
        , styleUnderline = attrUnderline > 0
        , styleReverse = attrReverse
        , styleDim = False
        }

-- | Convert libvterm color to our Color type
vtColorToColor :: VT.Color -> Color
vtColorToColor = \case
    VT.ColorDefault -> DefaultColor
    VT.ColorRGB r g b -> ColorRGB r g b
    VT.ColorIndex i -> Color256 i
