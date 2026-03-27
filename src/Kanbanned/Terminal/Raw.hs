{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- |
Module      : Kanbanned.Terminal.Raw
Description : Raw terminal mode and size detection
-}
module Kanbanned.Terminal.Raw
    ( withRawMode
    , getTerminalSize
    , TermSize (..)
    ) where

import Control.Exception (bracket)
import Foreign.C.Types (CInt (..), CUShort (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..), peekByteOff, pokeByteOff)
import System.IO qualified as IO
import System.Posix.IO (stdInput)
import System.Posix.Terminal
    ( TerminalAttributes
    , getTerminalAttributes
    , setTerminalAttributes
    , withoutMode
    )
import System.Posix.Terminal qualified as Posix

-- | Terminal dimensions
data TermSize = TermSize
    { tsRows :: !Int
    , tsCols :: !Int
    }
    deriving stock (Show, Eq)

-- | Winsize struct matching C struct winsize
data Winsize = Winsize
    { wsRow :: !CUShort
    , wsCol :: !CUShort
    , wsXPixel :: !CUShort
    , wsYPixel :: !CUShort
    }

instance Storable Winsize where
    sizeOf _ = 8
    alignment _ = 2
    peek ptr = do
        r <- peekByteOff ptr 0
        c <- peekByteOff ptr 2
        xp <- peekByteOff ptr 4
        yp <- peekByteOff ptr 6
        pure $ Winsize r c xp yp
    poke ptr (Winsize r c xp yp) = do
        pokeByteOff ptr 0 r
        pokeByteOff ptr 2 c
        pokeByteOff ptr 4 xp
        pokeByteOff ptr 6 yp

foreign import ccall unsafe "sys/ioctl.h ioctl"
    c_ioctl :: CInt -> CInt -> Ptr Winsize -> IO CInt

-- | Get current terminal size via TIOCGWINSZ ioctl
getTerminalSize :: IO TermSize
getTerminalSize = alloca $ \ptr -> do
    -- 0x5413 = TIOCGWINSZ on Linux
    _ <- c_ioctl 1 0x5413 ptr
    ws <- peek ptr
    pure
        TermSize
            { tsRows = fromIntegral (wsRow ws)
            , tsCols = fromIntegral (wsCol ws)
            }

-- | Enter raw mode, run action, restore on exit
withRawMode :: IO a -> IO a
withRawMode action = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetBuffering IO.stdout IO.NoBuffering
    bracket getAndSetRaw restoreAttrs (const action)
  where
    getAndSetRaw :: IO TerminalAttributes
    getAndSetRaw = do
        attrs <- getTerminalAttributes stdInput
        let raw =
                foldl
                    withoutMode
                    attrs
                    [ Posix.EnableEcho
                    , Posix.ProcessInput
                    , Posix.KeyboardInterrupts
                    , Posix.StartStopOutput
                    , Posix.ExtendedFunctions
                    ]
        setTerminalAttributes
            stdInput
            raw
            Posix.Immediately
        pure attrs

    restoreAttrs :: TerminalAttributes -> IO ()
    restoreAttrs attrs =
        setTerminalAttributes
            stdInput
            attrs
            Posix.Immediately
