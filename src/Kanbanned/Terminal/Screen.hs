{- |
Module      : Kanbanned.Terminal.Screen
Description : Screen buffer management and flush to stdout
-}
module Kanbanned.Terminal.Screen
    ( Screen (..)
    , newScreen
    , renderToScreen
    , flushScreen
    ) where

import Data.ByteString.Builder (Builder, hPutBuilder)
import Kanbanned.Terminal.ANSI
    ( clearScreen
    , enterAltScreen
    , hideCursor
    , leaveAltScreen
    , moveTo
    , resetStyle
    , showCursor
    )
import Kanbanned.Terminal.Raw (TermSize (..))
import System.IO (stdout)

-- | Screen state
newtype Screen = Screen
    { screenSize :: TermSize
    }
    deriving stock (Show)

-- | Create a new screen with given dimensions
newScreen :: TermSize -> IO Screen
newScreen size = do
    hPutBuilder stdout $
        enterAltScreen <> hideCursor <> clearScreen
    pure Screen{screenSize = size}

-- | Render a builder to the screen (atomic write)
renderToScreen :: Screen -> Builder -> IO ()
renderToScreen _ content = do
    hPutBuilder stdout $
        hideCursor <> content <> resetStyle

-- | Flush screen and restore terminal
flushScreen :: Screen -> IO ()
flushScreen _ = do
    hPutBuilder stdout $
        resetStyle
            <> showCursor
            <> leaveAltScreen
            <> moveTo 1 1
