{- |
Module      : Kanbanned.Terminal.Input
Description : Terminal input parsing (ANSI escape sequences to key events)
-}
module Kanbanned.Terminal.Input
    ( Key (..)
    , Modifier (..)
    , Event (..)
    , readEvent
    , noMod
    , ctrl
    , alt
    ) where

import Data.Bits (shiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (chr, ord)
import Data.Word (Word8)
import System.IO
    ( hReady
    , stdin
    )

-- | Keyboard key
data Key
    = KeyChar !Char
    | KeyEnter
    | KeyTab
    | KeyBackspace
    | KeyEscape
    | KeyUp
    | KeyDown
    | KeyLeft
    | KeyRight
    | KeyHome
    | KeyEnd
    | KeyPageUp
    | KeyPageDown
    | KeyDelete
    | KeyInsert
    | KeyF !Int
    deriving stock (Show, Eq, Ord)

-- | Modifier flags
data Modifier = Modifier
    { modCtrl :: !Bool
    , modAlt :: !Bool
    , modShift :: !Bool
    }
    deriving stock (Show, Eq, Ord)

-- | Input event
data Event
    = KeyEvent !Key !Modifier
    | ResizeEvent
    | UnknownInput !ByteString
    deriving stock (Show, Eq)

-- | No modifiers
noMod :: Modifier
noMod = Modifier False False False

-- | Ctrl modifier
ctrl :: Modifier
ctrl = noMod{modCtrl = True}

-- | Alt modifier
alt :: Modifier
alt = noMod{modAlt = True}

-- | Read a single event from stdin (blocking)
readEvent :: IO Event
readEvent = do
    b <- BS.hGet stdin 1
    case BS.unpack b of
        [0x1B] -> do
            more <- hReady stdin
            if more
                then parseEscape
                else pure $ KeyEvent KeyEscape noMod
        [0x0D] -> pure $ KeyEvent KeyEnter noMod
        [0x09] -> pure $ KeyEvent KeyTab noMod
        [0x7F] -> pure $ KeyEvent KeyBackspace noMod
        [c]
            | c < 0x20 ->
                pure $
                    KeyEvent
                        (KeyChar $ chr $ fromIntegral c + 0x40)
                        ctrl
            | otherwise ->
                parseUtf8 c
        _ -> pure $ UnknownInput b

-- | Parse a UTF-8 character starting with the given byte
parseUtf8 :: Word8 -> IO Event
parseUtf8 b
    | b .&. 0x80 == 0 =
        pure $ KeyEvent (KeyChar $ chr $ fromIntegral b) noMod
    | b .&. 0xE0 == 0xC0 = readUtf8Cont 1 (fromIntegral $ b .&. 0x1F)
    | b .&. 0xF0 == 0xE0 = readUtf8Cont 2 (fromIntegral $ b .&. 0x0F)
    | b .&. 0xF8 == 0xF0 = readUtf8Cont 3 (fromIntegral $ b .&. 0x07)
    | otherwise = pure $ UnknownInput (BS.singleton b)
  where
    readUtf8Cont :: Int -> Int -> IO Event
    readUtf8Cont 0 acc =
        pure $ KeyEvent (KeyChar $ chr acc) noMod
    readUtf8Cont n acc = do
        rest <- BS.hGet stdin 1
        case BS.unpack rest of
            [c]
                | c .&. 0xC0 == 0x80 ->
                    readUtf8Cont
                        (n - 1)
                        ( shiftL acc 6
                            .|. fromIntegral (c .&. 0x3F)
                        )
            _ -> pure $ UnknownInput (BS.singleton b <> rest)

-- | Parse escape sequence after ESC
parseEscape :: IO Event
parseEscape = do
    b <- BS.hGet stdin 1
    case BS.unpack b of
        [0x5B] -> parseCsi -- ESC [
        [0x4F] -> parseSS3 -- ESC O
        [c] ->
            -- Alt + key
            pure $
                KeyEvent
                    (KeyChar $ chr $ fromIntegral c)
                    alt
        _ -> pure $ UnknownInput (BS.cons 0x1B b)

-- | Parse CSI sequence (ESC [ ...)
parseCsi :: IO Event
parseCsi = do
    -- Read parameter bytes and final byte
    (params, final) <- readCsiParams BS.empty
    let paramStr = params
    case BS.unpack (BS.singleton final) of
        [0x41] -> pure $ KeyEvent KeyUp (csiMod paramStr)
        [0x42] -> pure $ KeyEvent KeyDown (csiMod paramStr)
        [0x43] -> pure $ KeyEvent KeyRight (csiMod paramStr)
        [0x44] -> pure $ KeyEvent KeyLeft (csiMod paramStr)
        [0x48] -> pure $ KeyEvent KeyHome (csiMod paramStr)
        [0x46] -> pure $ KeyEvent KeyEnd (csiMod paramStr)
        [0x7E] -> parseTilde paramStr
        _ ->
            pure $
                UnknownInput $
                    BS.pack [0x1B, 0x5B]
                        <> params
                        <> BS.singleton final

-- | Read CSI parameter bytes until final byte
readCsiParams :: ByteString -> IO (ByteString, Word8)
readCsiParams acc = do
    b <- BS.hGet stdin 1
    case BS.unpack b of
        [c]
            | c >= 0x30 && c <= 0x3F ->
                readCsiParams (acc <> b)
            | otherwise -> pure (acc, c)
        _ -> pure (acc, 0)

-- | Parse CSI n ~ sequences
parseTilde :: ByteString -> IO Event
parseTilde params =
    case parseNum params of
        Just (1, _) -> pure $ KeyEvent KeyHome noMod
        Just (2, _) -> pure $ KeyEvent KeyInsert noMod
        Just (3, _) -> pure $ KeyEvent KeyDelete noMod
        Just (4, _) -> pure $ KeyEvent KeyEnd noMod
        Just (5, _) -> pure $ KeyEvent KeyPageUp noMod
        Just (6, _) -> pure $ KeyEvent KeyPageDown noMod
        Just (15, _) -> pure $ KeyEvent (KeyF 5) noMod
        Just (17, _) -> pure $ KeyEvent (KeyF 6) noMod
        Just (18, _) -> pure $ KeyEvent (KeyF 7) noMod
        Just (19, _) -> pure $ KeyEvent (KeyF 8) noMod
        Just (20, _) -> pure $ KeyEvent (KeyF 9) noMod
        Just (21, _) -> pure $ KeyEvent (KeyF 10) noMod
        Just (23, _) -> pure $ KeyEvent (KeyF 11) noMod
        Just (24, _) -> pure $ KeyEvent (KeyF 12) noMod
        _ ->
            pure $
                UnknownInput $
                    BS.pack [0x1B, 0x5B]
                        <> params
                        <> BS.singleton 0x7E

-- | Parse SS3 sequences (ESC O ...)
parseSS3 :: IO Event
parseSS3 = do
    b <- BS.hGet stdin 1
    case BS.unpack b of
        [0x50] -> pure $ KeyEvent (KeyF 1) noMod
        [0x51] -> pure $ KeyEvent (KeyF 2) noMod
        [0x52] -> pure $ KeyEvent (KeyF 3) noMod
        [0x53] -> pure $ KeyEvent (KeyF 4) noMod
        _ ->
            pure $
                UnknownInput $
                    BS.pack [0x1B, 0x4F] <> b

-- | Extract modifier from CSI params (;N format)
csiMod :: ByteString -> Modifier
csiMod params =
    case parseSemicolon params of
        Just (_, m) -> decodeModifier m
        Nothing -> noMod

-- | Parse "N;M" from params
parseSemicolon :: ByteString -> Maybe (Int, Int)
parseSemicolon bs =
    case BS.split (fromIntegral $ ord ';') bs of
        [a, b'] -> do
            n <- readInt a
            m <- readInt b'
            Just (n, m)
        _ -> Nothing

-- | Parse leading number from params
parseNum :: ByteString -> Maybe (Int, ByteString)
parseNum bs
    | BS.null bs = Nothing
    | otherwise =
        case BS.span (\c -> c >= 0x30 && c <= 0x39) bs of
            (digits, rest)
                | BS.null digits -> Nothing
                | otherwise -> do
                    n <- readInt digits
                    Just (n, rest)

-- | Read integer from ASCII digit bytes
readInt :: ByteString -> Maybe Int
readInt bs
    | BS.null bs = Nothing
    | otherwise =
        Just $
            BS.foldl'
                (\acc c -> acc * 10 + fromIntegral c - 48)
                0
                bs

-- | Decode xterm modifier number
decodeModifier :: Int -> Modifier
decodeModifier n =
    let m = n - 1
    in  Modifier
            { modCtrl = m .&. 4 /= 0
            , modAlt = m .&. 2 /= 0
            , modShift = m .&. 1 /= 0
            }
