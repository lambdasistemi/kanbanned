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
import Data.IORef (readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Graphics.Vty qualified as V
import Kanbanned.Agent.Rest (launchSession)
import Kanbanned.Agent.Types (AgentSession (..))
import Kanbanned.Agent.WebSocket (sendTerminalInput)
import Kanbanned.App.Env (Env (..), TerminalState (..))
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
    , ItemView (..)
    , Name
    , Page (..)
    , currentColumnItems
    , selectedItemSessionId
    )

-- | Top-level event handler
handleEvent
    :: Env
    -> BrickEvent Name AppEvent
    -> EventM Name AppState ()
handleEvent env (VtyEvent (V.EvKey key mods)) = do
    st <- get
    if stTerminalFocused st
        then handleTerminalKey env key mods
        else handleKanbanKey env key mods
handleEvent _env (VtyEvent (V.EvMouseDown _col _row btn _mods)) =
    case btn of
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
-- Terminal mode (keyboard focused on terminal)
------------------------------------------------------------------------

handleTerminalKey
    :: Env -> V.Key -> [V.Modifier] -> EventM Name AppState ()
handleTerminalKey env key mods
    | isCtrlRBracket key mods =
        modify $ \s -> s{stTerminalFocused = False}
    | otherwise = do
        st <- get
        case selectedItemSessionId st of
            Just sid -> do
                terminals <- liftIO $ readIORef (envTerminals env)
                case Map.lookup sid terminals of
                    Just ts ->
                        liftIO $
                            sendTerminalInput
                                (tsConn ts)
                                (vtyKeyToBytes key mods)
                    Nothing -> pure ()
            Nothing -> pure ()

isCtrlRBracket :: V.Key -> [V.Modifier] -> Bool
isCtrlRBracket (V.KChar '\x1d') [] = True
isCtrlRBracket (V.KChar ']') [V.MCtrl] = True
isCtrlRBracket _ _ = False

------------------------------------------------------------------------
-- Kanban mode
------------------------------------------------------------------------

handleKanbanKey
    :: Env -> V.Key -> [V.Modifier] -> EventM Name AppState ()
handleKanbanKey env key mods
    | mods /= [] = pure ()
    | otherwise = case key of
        V.KChar 'q' -> halt
        -- Column navigation
        V.KChar 'h' -> modify prevPage
        V.KLeft -> modify prevPage
        V.KChar 'l' -> modify nextPage
        V.KRight -> modify nextPage
        -- Item navigation
        V.KChar 'j' -> modify (moveSelection 1)
        V.KDown -> modify (moveSelection 1)
        V.KChar 'k' -> modify (moveSelection (-1))
        V.KUp -> modify (moveSelection (-1))
        -- Tab: toggle current item between description and terminal
        V.KChar '\t' -> modify toggleItemView
        -- Enter: focus terminal if showing terminal view
        V.KEnter -> do
            st <- get
            case selectedItemSessionId st of
                Just sid ->
                    case Map.findWithDefault ShowDescription sid (stItemViews st) of
                        ShowTerminal ->
                            modify $ \s -> s{stTerminalFocused = True}
                        ShowDescription -> pure ()
                Nothing -> pure ()
        -- Move item
        V.KChar 'm' -> handleMoveItem env
        -- Agent: launch new session
        V.KChar 'a' -> handleLaunchAgent env
        -- Settings
        V.KChar 's' ->
            modify $ \s -> s{stPage = SettingsPage}
        -- Refresh
        V.KChar 'r' ->
            liftIO $ void $ forkIO $ refreshData env
        _ -> pure ()

------------------------------------------------------------------------
-- Toggle item view (description ↔ terminal)
------------------------------------------------------------------------

toggleItemView :: AppState -> AppState
toggleItemView s =
    case selectedItemSessionId s of
        Just sid ->
            let current =
                    Map.findWithDefault
                        ShowDescription
                        sid
                        (stItemViews s)
                next = case current of
                    ShowDescription -> ShowTerminal
                    ShowTerminal -> ShowDescription
            in  s
                    { stItemViews =
                        Map.insert sid next (stItemViews s)
                    , stTerminalFocused =
                        next == ShowTerminal
                    }
        Nothing -> s

------------------------------------------------------------------------
-- Launch agent session
------------------------------------------------------------------------

handleLaunchAgent :: Env -> EventM Name AppState ()
handleLaunchAgent env = do
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
                if Map.member sid (stSessions st)
                    then
                        -- Already has session — switch to terminal view
                        modify $ \s ->
                            s
                                { stItemViews =
                                    Map.insert
                                        sid
                                        ShowTerminal
                                        (stItemViews s)
                                , stTerminalFocused = True
                                }
                    else do
                        -- Launch new session
                        modify $ \s ->
                            s
                                { stToast =
                                    Just
                                        ("Launching " <> sid)
                                }
                        liftIO $
                            void $
                                forkIO $ do
                                    result <-
                                        launchSession
                                            (envAgentClient env)
                                            owner
                                            repo
                                            issue
                                    case result of
                                        Right session ->
                                            writeChan env $
                                                StateUpdate $ \s ->
                                                    s
                                                        { stSessions =
                                                            Map.insert
                                                                (asId session)
                                                                session
                                                                (stSessions s)
                                                        , stToast =
                                                            Just
                                                                ( "Launched "
                                                                    <> asId session
                                                                    <> " — attaching..."
                                                                )
                                                        }
                                        Left e ->
                                            writeChan
                                                env
                                                (ErrorEvent e)
                                    -- Refresh will auto-attach
                                    refreshData env
        _ ->
            modify $ \s ->
                s{stToast = Just "Select an issue first"}

------------------------------------------------------------------------
-- Move item between columns
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
                mSf = Map.lookup pid (stStatusFields st)
            case mSf of
                Just sf -> do
                    let tText = statusToText target
                        mOpt =
                            filter
                                (\o -> soName o == tText)
                                (sfOptions sf)
                    case mOpt of
                        (opt : _) -> do
                            modify $ \s ->
                                ( updateItemInState
                                    pid
                                    (itemId item)
                                    (\i -> i{itemStatus = Just target})
                                    s
                                )
                                    { stSelectedIndex = 0
                                    , stToast =
                                        Just ("Moving to " <> tText)
                                    }
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
                                                writeChan
                                                    env
                                                    ( StateUpdate $
                                                        updateItemInState
                                                            pid
                                                            (itemId item)
                                                            ( \i ->
                                                                i
                                                                    { itemStatus =
                                                                        itemStatus item
                                                                    }
                                                            )
                                                    )
                                                writeChan
                                                    env
                                                    (ErrorEvent e)
                        [] ->
                            modify $ \s ->
                                s
                                    { stToast =
                                        Just ("No option for " <> tText)
                                    }
                Nothing ->
                    modify $ \s ->
                        s{stToast = Just "No status field"}
        _ ->
            modify $ \s ->
                s{stToast = Just "Select an issue first"}

------------------------------------------------------------------------
-- State transformations
------------------------------------------------------------------------

moveSelection :: Int -> AppState -> AppState
moveSelection delta s =
    let items = currentColumnItems s
        maxIdx = max 0 (length items - 1)
        newIdx =
            max 0 (min maxIdx (stSelectedIndex s + delta))
    in  s{stSelectedIndex = newIdx}

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
-- Item update helper
------------------------------------------------------------------------

updateItemInState
    :: T.Text
    -> T.Text
    -> (ProjectItem -> ProjectItem)
    -> AppState
    -> AppState
updateItemInState pid targetId f s =
    let is = Map.findWithDefault [] pid (stItems s)
        is' =
            map
                (\i -> if itemId i == targetId then f i else i)
                is
    in  s{stItems = Map.insert pid is' (stItems s)}

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

writeChan :: Env -> AppEvent -> IO ()
writeChan env = Brick.BChan.writeBChan (envChan env)

safeIndex :: Int -> [a] -> Maybe a
safeIndex _ [] = Nothing
safeIndex 0 (x : _) = Just x
safeIndex n (_ : xs)
    | n > 0 = safeIndex (n - 1) xs
    | otherwise = Nothing

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
