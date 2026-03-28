{- |
Module      : Kanbanned.App.Event
Description : Event handling for the brick app
-}
module Kanbanned.App.Event
    ( handleEvent
    ) where

import Brick
    ( BrickEvent (..)
    , EventM
    , get
    , halt
    , modify
    )
import Brick.BChan qualified
import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.IORef (readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Foreign.C.Types (CInt (..), CUShort)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import Graphics.Vty qualified as V
import Kanbanned.Agent.Rest (launchSession)
import Kanbanned.Agent.Types (AgentSession (..))
import Kanbanned.Agent.WebSocket
    ( TerminalConnection
    , closeTerminal
    , connectTerminal
    , receiveTerminalOutput
    , sendTerminalInput
    , sendTerminalResize
    )
import Kanbanned.App.Env (Env (..))
import Kanbanned.App.Refresh (refreshData)
import Kanbanned.Config (Config (..))
import Kanbanned.GitHub.GraphQL (updateItemStatus)
import Kanbanned.GitHub.Types
    ( KanbanStatus (..)
    , ProjectItem (..)
    , StatusField (..)
    , StatusOption (..)
    , statusToText
    )
import Kanbanned.State
    ( AppEvent (..)
    , AppState (..)
    , Name
    , Page (..)
    , currentColumnItems
    )
import Kanbanned.UI.Terminal
    ( TerminalView
    , feedTerminalView
    , freeTerminalView
    , getTerminalImage
    , newTerminalView
    )
import System.Timeout (timeout)
import Text.Read (readMaybe)

-- | Top-level event handler
handleEvent
    :: Env
    -> BrickEvent Name AppEvent
    -> EventM Name AppState ()
handleEvent env (VtyEvent (V.EvKey key mods)) = do
    st <- get
    if stTerminalActive st
        then handleTerminalKey env key mods
        else handleKanbanKey env key mods
handleEvent env (VtyEvent (V.EvMouseDown _col _row btn _mods)) = do
    st <- get
    if stTerminalActive st
        then case btn of
            V.BScrollUp -> liftIO $ do
                mConn <- readIORef (envTermConn env)
                -- SGR 1006: button 64 = scroll up
                mapM_ (`sendTerminalInput` "\ESC[<64;1;1M") mConn
            V.BScrollDown -> liftIO $ do
                mConn <- readIORef (envTermConn env)
                -- SGR 1006: button 65 = scroll down
                mapM_ (`sendTerminalInput` "\ESC[<65;1;1M") mConn
            _ -> pure ()
        else case btn of
            V.BScrollUp -> modify (moveSelection (-3))
            V.BScrollDown -> modify (moveSelection 3)
            _ -> pure ()
handleEvent _ (VtyEvent V.EvMouseUp{}) = pure ()
handleEvent _ (VtyEvent V.EvResize{}) = pure ()
handleEvent _ (AppEvent (StateUpdate f)) = modify f
handleEvent _ (AppEvent (ToastEvent msg)) =
    modify $ \s -> s{stToast = Just msg}
handleEvent _ (AppEvent (ErrorEvent msg)) =
    modify $ \s -> s{stToast = Just ("ERROR: " <> msg)}
handleEvent _ _ = pure ()

------------------------------------------------------------------------
-- Terminal mode
------------------------------------------------------------------------

handleTerminalKey
    :: Env
    -> V.Key
    -> [V.Modifier]
    -> EventM Name AppState ()
handleTerminalKey env key mods
    -- Ctrl-] (0x1D = GS) — detach from terminal
    | isCtrlRBracket key mods =
        modify $ \s -> s{stTerminalActive = False}
    | otherwise = do
        let bs = vtyKeyToBytes key mods
        mConn <- liftIO $ readIORef (envTermConn env)
        case mConn of
            Just conn ->
                liftIO $ sendTerminalInput conn bs
            Nothing ->
                modify $ \s ->
                    s{stToast = Just "No terminal connection"}

{- | Detect Ctrl-] which vty delivers as either
KChar '\x1d' (raw GS byte) or KChar ']' with MCtrl
-}
isCtrlRBracket :: V.Key -> [V.Modifier] -> Bool
isCtrlRBracket (V.KChar '\x1d') [] = True
isCtrlRBracket (V.KChar ']') [V.MCtrl] = True
isCtrlRBracket _ _ = False

------------------------------------------------------------------------
-- Kanban mode
------------------------------------------------------------------------

handleKanbanKey
    :: Env
    -> V.Key
    -> [V.Modifier]
    -> EventM Name AppState ()
handleKanbanKey env key mods
    | mods /= [] = pure ()
    | otherwise = case key of
        V.KChar 'q' -> halt
        V.KChar 'h' -> modify prevPage
        V.KLeft -> modify prevPage
        V.KChar 'l' -> modify nextPage
        V.KRight -> modify nextPage
        V.KChar 'j' -> modify (moveSelection 1)
        V.KDown -> modify (moveSelection 1)
        V.KChar 'k' -> modify (moveSelection (-1))
        V.KUp -> modify (moveSelection (-1))
        V.KEnter -> modify toggleExpand
        V.KChar '\t' ->
            modify $ \s ->
                s
                    { stTerminalActive =
                        not (stTerminalActive s)
                            && isJust (stActiveTerminal s)
                    }
        V.KChar 'm' -> handleMoveItem env
        V.KChar 'a' -> handleAgentAction env
        V.KChar 's' ->
            modify $ \s -> s{stPage = SettingsPage}
        V.KChar 'r' ->
            liftIO $ void $ forkIO $ refreshData env
        _ -> pure ()

------------------------------------------------------------------------
-- Move item (cycle Backlog→WIP→Done→Backlog)
------------------------------------------------------------------------

handleMoveItem :: Env -> EventM Name AppState ()
handleMoveItem env = do
    st <- get
    let items = currentColumnItems st
        mItem = safeIndex (stSelectedIndex st) items
        mPid = cfgProjectId (stConfig st)
    case (mItem, envGhClient env, mPid) of
        (Just item, Just ghClient, Just pid) -> do
            let target = case itemStatus item of
                    Just Backlog -> WIP
                    Just WIP -> Done
                    Just Done -> Backlog
                    Nothing -> Backlog
                mSf =
                    Map.lookup pid (stStatusFields st)
            case mSf of
                Just sf -> do
                    let tText = statusToText target
                        mOpt =
                            filter
                                (\o -> soName o == tText)
                                (sfOptions sf)
                    case mOpt of
                        (opt : _) -> do
                            -- Optimistic update
                            modify $ \s ->
                                let is =
                                        Map.findWithDefault
                                            []
                                            pid
                                            (stItems s)
                                    is' =
                                        map
                                            ( \i ->
                                                if itemId i
                                                    == itemId item
                                                    then
                                                        i
                                                            { itemStatus =
                                                                Just target
                                                            }
                                                    else i
                                            )
                                            is
                                in  s
                                        { stItems =
                                            Map.insert
                                                pid
                                                is'
                                                (stItems s)
                                        , stSelectedIndex = 0
                                        , stToast =
                                            Just
                                                ( "Moving to "
                                                    <> tText
                                                )
                                        }
                            -- API call in background
                            liftIO $
                                void $
                                    forkIO $ do
                                        result <-
                                            updateItemStatus
                                                ghClient
                                                pid
                                                (itemId item)
                                                (sfFieldId sf)
                                                (soId opt)
                                        case result of
                                            Right () ->
                                                writeChan
                                                    env
                                                    ( ToastEvent $
                                                        "Moved to "
                                                            <> tText
                                                    )
                                            Left e -> do
                                                -- Rollback
                                                writeChan
                                                    env
                                                    ( StateUpdate $ \s ->
                                                        let is =
                                                                Map.findWithDefault
                                                                    []
                                                                    pid
                                                                    (stItems s)
                                                            is' =
                                                                map
                                                                    ( \i ->
                                                                        if itemId i
                                                                            == itemId item
                                                                            then
                                                                                i
                                                                                    { itemStatus =
                                                                                        itemStatus item
                                                                                    }
                                                                            else i
                                                                    )
                                                                    is
                                                        in  s
                                                                { stItems =
                                                                    Map.insert
                                                                        pid
                                                                        is'
                                                                        (stItems s)
                                                                }
                                                    )
                                                writeChan
                                                    env
                                                    (ErrorEvent e)
                        [] ->
                            modify $ \s ->
                                s
                                    { stToast =
                                        Just
                                            ( "No option for "
                                                <> tText
                                            )
                                    }
                Nothing ->
                    modify $ \s ->
                        s{stToast = Just "No status field"}
        _ ->
            modify $ \s ->
                s{stToast = Just "Select an issue first"}

------------------------------------------------------------------------
-- Agent launch/attach
------------------------------------------------------------------------

handleAgentAction :: Env -> EventM Name AppState ()
handleAgentAction env = do
    st <- get
    let items = currentColumnItems st
        mItem = safeIndex (stSelectedIndex st) items
    case mItem of
        Just item
            | Just owner <- itemRepoOwner item
            , Just repo <- itemRepoName item
            , Just issue <- itemNumber item -> do
                let sid =
                        repo <> "-" <> T.pack (show issue)
                modify $ \s ->
                    s{stToast = Just ("Connecting " <> sid)}
                liftIO $
                    void $
                        forkIO $ case Map.lookup sid (stSessions st) of
                            Nothing -> do
                                result <-
                                    launchSession
                                        (envAgentClient env)
                                        owner
                                        repo
                                        issue
                                case result of
                                    Right session -> do
                                        writeChan env $
                                            StateUpdate $ \s ->
                                                s
                                                    { stSessions =
                                                        Map.insert
                                                            (asId session)
                                                            session
                                                            (stSessions s)
                                                    }
                                        attachTerminal env sid
                                    Left e ->
                                        writeChan env (ErrorEvent e)
                            Just _ -> attachTerminal env sid
        _ ->
            modify $ \s ->
                s{stToast = Just "Select an issue first"}

-- | Attach to a running session's terminal (runs in background)
attachTerminal :: Env -> T.Text -> IO ()
attachTerminal env sessionId = do
    -- Clean up old
    readIORef (envTermView env)
        >>= mapM_ freeTerminalView
    readIORef (envTermConn env)
        >>= mapM_ closeTerminal
    writeIORef (envTermView env) Nothing
    writeIORef (envTermConn env) Nothing
    -- Get terminal size — use half width for split pane
    (termW, termH) <- getTermSize
    let cols = max 40 (termW `div` 2)
        rows = max 10 (termH - 2)
    tv <- newTerminalView rows cols (Just sessionId)
    let (host, port) =
            parseHostPort
                (cfgAgentServer (envConfig env))
    -- Timeout after 10 seconds
    mResult <-
        timeout 10_000_000 $
            connectTerminal host port sessionId
    case mResult of
        Just (Right conn) -> do
            sendTerminalResize conn cols rows
            writeIORef (envTermView env) (Just tv)
            writeIORef (envTermConn env) (Just conn)
            writeChan env $
                StateUpdate $ \s ->
                    s
                        { stTerminalActive = True
                        , stActiveTerminal = Just sessionId
                        , stToast =
                            Just
                                ("Attached to " <> sessionId)
                        }
            void $ forkIO $ terminalReceiveLoop env tv conn
        Just (Left e) -> do
            freeTerminalView tv
            writeChan env $
                ErrorEvent ("Connect failed: " <> e)
        Nothing -> do
            freeTerminalView tv
            writeChan env $
                ErrorEvent "Connection timed out"

{- | Background loop receiving terminal output.
Feeds bytes into libvterm and nudges brick to re-render,
but only if there isn't already a pending redraw.
-}
terminalReceiveLoop
    :: Env -> TerminalView -> TerminalConnection -> IO ()
terminalReceiveLoop env tv conn = go
  where
    go = do
        mData <- receiveTerminalOutput conn
        case mData of
            Just bs -> do
                feedTerminalView tv bs
                img <- getTerminalImage tv
                -- Non-blocking write — drop if channel full
                void $
                    Brick.BChan.writeBChanNonBlocking
                        (envChan env)
                        ( StateUpdate $ \s ->
                            s{stTerminalImage = Just img}
                        )
                go
            Nothing ->
                writeChan env $
                    StateUpdate $ \s ->
                        s
                            { stTerminalActive = False
                            , stActiveTerminal = Nothing
                            }

------------------------------------------------------------------------
-- State transformations
------------------------------------------------------------------------

moveSelection :: Int -> AppState -> AppState
moveSelection delta s =
    let items = currentColumnItems s
        maxIdx = max 0 (length items - 1)
        newIdx =
            max 0 (min maxIdx (stSelectedIndex s + delta))
        newExpanded =
            if isJust (stExpandedItem s)
                then fmap itemId (safeIndex newIdx items)
                else Nothing
    in  s
            { stSelectedIndex = newIdx
            , stExpandedItem = newExpanded
            }

toggleExpand :: AppState -> AppState
toggleExpand s =
    let items = currentColumnItems s
        mItem = safeIndex (stSelectedIndex s) items
    in  case mItem of
            Just item
                | stExpandedItem s == Just (itemId item) ->
                    s{stExpandedItem = Nothing}
                | otherwise ->
                    s{stExpandedItem = Just (itemId item)}
            Nothing -> s

prevPage :: AppState -> AppState
prevPage s =
    s{stPage = prev (stPage s), stSelectedIndex = 0}
  where
    prev BacklogPage = DonePage
    prev WIPPage = BacklogPage
    prev DonePage = WIPPage
    prev SettingsPage = DonePage

nextPage :: AppState -> AppState
nextPage s =
    s{stPage = next (stPage s), stSelectedIndex = 0}
  where
    next BacklogPage = WIPPage
    next WIPPage = DonePage
    next DonePage = BacklogPage
    next SettingsPage = BacklogPage

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

writeChan :: Env -> AppEvent -> IO ()
writeChan env =
    Brick.BChan.writeBChan (envChan env)

-- | Get terminal size (cols, rows) via ioctl
foreign import ccall unsafe "sys/ioctl.h ioctl"
    c_ioctl :: CInt -> CInt -> Ptr a -> IO CInt

getTermSize :: IO (Int, Int)
getTermSize =
    allocaBytes 8 $ \ptr -> do
        _ <- c_ioctl 1 0x5413 ptr
        rows <- peekByteOff ptr 0 :: IO CUShort
        cols <- peekByteOff ptr 2 :: IO CUShort
        pure (fromIntegral cols, fromIntegral rows)

safeIndex :: Int -> [a] -> Maybe a
safeIndex _ [] = Nothing
safeIndex 0 (x : _) = Just x
safeIndex n (_ : xs)
    | n > 0 = safeIndex (n - 1) xs
    | otherwise = Nothing

parseHostPort :: T.Text -> (T.Text, Int)
parseHostPort url =
    let stripped =
            T.dropWhile (== '/') $
                T.drop 1 $
                    T.dropWhile (/= ':') url
    in  case T.splitOn ":" stripped of
            [h, p] ->
                ( resolveHost h
                , fromMaybe 8080 (readMaybe (T.unpack p))
                )
            [h] -> (resolveHost h, 8080)
            _ -> ("127.0.0.1", 8080)
  where
    resolveHost h
        | h == "localhost" = "127.0.0.1"
        | otherwise = h

vtyKeyToBytes :: V.Key -> [V.Modifier] -> ByteString
vtyKeyToBytes key _mods = case key of
    V.KChar c -> TE.encodeUtf8 (T.singleton c)
    V.KEnter -> "\r"
    V.KBS -> "\x7f"
    V.KEsc -> "\x1b"
    V.KUp -> "\x1b[A"
    V.KDown -> "\x1b[B"
    V.KRight -> "\x1b[C"
    V.KLeft -> "\x1b[D"
    V.KHome -> "\x1b[H"
    V.KEnd -> "\x1b[F"
    V.KPageUp -> "\x1b[5~"
    V.KPageDown -> "\x1b[6~"
    V.KDel -> "\x1b[3~"
    V.KIns -> "\x1b[2~"
    V.KFun n ->
        "\x1b["
            <> TE.encodeUtf8 (T.pack (show (n + 10)))
            <> "~"
    _ -> ""
