{- |
Module      : Kanbanned.Terminal.ANSI
Description : ANSI escape sequence generation for terminal rendering
-}
module Kanbanned.Terminal.ANSI
    ( -- * Cursor
      moveTo
    , hideCursor
    , showCursor
    , saveCursor
    , restoreCursor

      -- * Screen
    , clearScreen
    , clearLine
    , enterAltScreen
    , leaveAltScreen
    , enableMouse
    , disableMouse

      -- * Style
    , Style (..)
    , Color (..)
    , defaultStyle
    , withStyle
    , resetStyle

      -- * Drawing
    , drawText
    , drawStyledText
    , drawBox
    , drawHLine
    , fillRect
    ) where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as B
import Data.Foldable (fold)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Word (Word8)

-- | Terminal color
data Color
    = DefaultColor
    | Black
    | Red
    | Green
    | Yellow
    | Blue
    | Magenta
    | Cyan
    | White
    | BrightBlack
    | BrightRed
    | BrightGreen
    | BrightYellow
    | BrightBlue
    | BrightMagenta
    | BrightCyan
    | BrightWhite
    | Color256 !Word8
    | ColorRGB !Word8 !Word8 !Word8
    deriving stock (Show, Eq)

-- | Text style
data Style = Style
    { styleFg :: !Color
    , styleBg :: !Color
    , styleBold :: !Bool
    , styleItalic :: !Bool
    , styleUnderline :: !Bool
    , styleReverse :: !Bool
    , styleDim :: !Bool
    }
    deriving stock (Show, Eq)

-- | Default style (no attributes, default colors)
defaultStyle :: Style
defaultStyle =
    Style
        { styleFg = DefaultColor
        , styleBg = DefaultColor
        , styleBold = False
        , styleItalic = False
        , styleUnderline = False
        , styleReverse = False
        , styleDim = False
        }

-- | Move cursor to row, col (1-based)
moveTo :: Int -> Int -> Builder
moveTo row col =
    csi <> intDec row <> B.char7 ';' <> intDec col <> B.char7 'H'

-- | Hide cursor
hideCursor :: Builder
hideCursor = csi <> B.string7 "?25l"

-- | Show cursor
showCursor :: Builder
showCursor = csi <> B.string7 "?25h"

-- | Save cursor position
saveCursor :: Builder
saveCursor = B.string7 "\ESC7"

-- | Restore cursor position
restoreCursor :: Builder
restoreCursor = B.string7 "\ESC8"

-- | Clear entire screen
clearScreen :: Builder
clearScreen = csi <> B.string7 "2J"

-- | Clear current line
clearLine :: Builder
clearLine = csi <> B.string7 "2K"

-- | Enter alternate screen buffer
enterAltScreen :: Builder
enterAltScreen = csi <> B.string7 "?1049h"

-- | Leave alternate screen buffer
leaveAltScreen :: Builder
leaveAltScreen = csi <> B.string7 "?1049l"

-- | Enable mouse tracking
enableMouse :: Builder
enableMouse = csi <> B.string7 "?1006h"

-- | Disable mouse tracking
disableMouse :: Builder
disableMouse = csi <> B.string7 "?1006l"

-- | Reset all styles
resetStyle :: Builder
resetStyle = csi <> B.string7 "0m"

-- | Apply style via SGR sequences
withStyle :: Style -> Builder
withStyle Style{..} =
    resetStyle
        <> fold
            [ if styleBold then sgr "1" else mempty
            , if styleDim then sgr "2" else mempty
            , if styleItalic then sgr "3" else mempty
            , if styleUnderline then sgr "4" else mempty
            , if styleReverse then sgr "7" else mempty
            , fgColor styleFg
            , bgColor styleBg
            ]

-- | Generate foreground color SGR
fgColor :: Color -> Builder
fgColor DefaultColor = mempty
fgColor c = colorSGR 30 38 c

-- | Generate background color SGR
bgColor :: Color -> Builder
bgColor DefaultColor = mempty
bgColor c = colorSGR 40 48 c

-- | Generate color SGR with base offset
colorSGR :: Int -> Int -> Color -> Builder
colorSGR base _ext c = case c of
    DefaultColor -> mempty
    Black -> sgr (show base)
    Red -> sgr (show $ base + 1)
    Green -> sgr (show $ base + 2)
    Yellow -> sgr (show $ base + 3)
    Blue -> sgr (show $ base + 4)
    Magenta -> sgr (show $ base + 5)
    Cyan -> sgr (show $ base + 6)
    White -> sgr (show $ base + 7)
    BrightBlack -> sgr (show $ base + 60)
    BrightRed -> sgr (show $ base + 61)
    BrightGreen -> sgr (show $ base + 62)
    BrightYellow -> sgr (show $ base + 63)
    BrightBlue -> sgr (show $ base + 64)
    BrightMagenta -> sgr (show $ base + 65)
    BrightCyan -> sgr (show $ base + 66)
    BrightWhite -> sgr (show $ base + 67)
    Color256 n ->
        csi
            <> intDec _ext
            <> B.string7 ";5;"
            <> intDec (fromIntegral n)
            <> B.char7 'm'
    ColorRGB r g b ->
        csi
            <> intDec _ext
            <> B.string7 ";2;"
            <> intDec (fromIntegral r)
            <> B.char7 ';'
            <> intDec (fromIntegral g)
            <> B.char7 ';'
            <> intDec (fromIntegral b)
            <> B.char7 'm'

-- | Draw text at position with default style
drawText :: Int -> Int -> Text -> Builder
drawText row col t =
    moveTo row col <> B.byteString (T.encodeUtf8 t)

-- | Draw text with a specific style
drawStyledText :: Int -> Int -> Style -> Text -> Builder
drawStyledText row col style t =
    moveTo row col
        <> withStyle style
        <> B.byteString (T.encodeUtf8 t)
        <> resetStyle

-- | Draw a box outline
drawBox :: Int -> Int -> Int -> Int -> Style -> Builder
drawBox row col h w style =
    withStyle style
        <> drawText row col topLine
        <> foldMap
            (\r -> drawText r col midLine)
            [row + 1 .. row + h - 2]
        <> drawText (row + h - 1) col botLine
        <> resetStyle
  where
    topLine = "\x250C" <> repeatText (w - 2) "\x2500" <> "\x2510"
    botLine = "\x2514" <> repeatText (w - 2) "\x2500" <> "\x2518"
    midLine = "\x2502" <> repeatText (w - 2) " " <> "\x2502"

-- | Draw a horizontal line
drawHLine :: Int -> Int -> Int -> Style -> Builder
drawHLine row col w style =
    drawStyledText row col style (repeatText w "\x2500")

-- | Fill a rectangle with spaces
fillRect :: Int -> Int -> Int -> Int -> Style -> Builder
fillRect row col h w style =
    withStyle style
        <> foldMap
            (\r -> moveTo r col <> B.byteString (T.encodeUtf8 spaces))
            [row .. row + h - 1]
        <> resetStyle
  where
    spaces = repeatText w " "

-- | CSI prefix
csi :: Builder
csi = B.string7 "\ESC["

-- | SGR sequence
sgr :: String -> Builder
sgr code = csi <> B.string7 code <> B.char7 'm'

-- | Int to decimal builder
intDec :: Int -> Builder
intDec = B.intDec

-- | Repeat text n times
repeatText :: Int -> Text -> Text
repeatText n t
    | n <= 0 = ""
    | otherwise = mconcat $ replicate n t
