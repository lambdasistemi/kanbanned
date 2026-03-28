{- |
Module      : Kanbanned.App
Description : Main application event loop
-}
module Kanbanned.App
    ( runApp
    , CliOverrides (..)
    , noOverrides
    ) where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, withAsync)
import Control.Concurrent.STM
    ( TVar
    , atomically
    , modifyTVar'
    , newTVarIO
    , readTVarIO
    )
import Control.Exception (finally)
import Control.Monad (forever, void)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Kanbanned.Agent.Rest
    ( AgentClient
    , launchSession
    , listBranches
    , listSessions
    , newAgentClient
    )
import Kanbanned.Agent.Types (AgentSession (..))
import Kanbanned.Agent.Types qualified
import Kanbanned.Agent.WebSocket
    ( TerminalConnection
    , closeTerminal
    , connectTerminal
    , receiveTerminalOutput
    , sendTerminalInput
    , sendTerminalResize
    )
import Kanbanned.Config
    ( Config (..)
    , loadConfig
    )
import Kanbanned.GitHub.GraphQL
    ( GitHubClient
    , fetchProjectItems
    , fetchProjects
    , newGitHubClient
    , updateItemStatus
    )
import Kanbanned.GitHub.Types
    ( KanbanStatus (..)
    , ProjectItem (..)
    , StatusField (..)
    , StatusOption (..)
    , statusToText
    )
import Kanbanned.State
    ( AppState (..)
    , Page (..)
    , Toast (..)
    , ToastLevel (..)
    , currentColumnItems
    , initialState
    )
import Kanbanned.Terminal.ANSI (clearScreen)
import Kanbanned.Terminal.Input
    ( Event (..)
    , Key (..)
    , Modifier (..)
    , noMod
    , readEvent
    )
import Kanbanned.Terminal.Raw
    ( getTerminalSize
    , withRawMode
    )
import Kanbanned.Terminal.Screen
    ( Screen
    , flushScreen
    , newScreen
    , renderToScreen
    )
import Kanbanned.UI.Kanban
    ( renderHeader
    , renderKanban
    , renderSettings
    )
import Kanbanned.UI.Layout (Rect (..), terminalRect)
import Kanbanned.UI.StatusBar (renderStatusBar)
import Kanbanned.UI.Terminal
    ( TerminalView
    , feedTerminalView
    , freeTerminalView
    , newTerminalView
    , renderTerminalView
    )
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.IO qualified as IO

-- | CLI overrides for config
data CliOverrides = CliOverrides
    { cliToken :: !(Maybe T.Text)
    , cliServer :: !(Maybe T.Text)
    , cliProject :: !(Maybe T.Text)
    }
    deriving stock (Show)

-- | No overrides
noOverrides :: CliOverrides
noOverrides = CliOverrides Nothing Nothing Nothing

-- | Apply CLI overrides to config
applyOverrides :: CliOverrides -> Config -> Config
applyOverrides CliOverrides{..} cfg =
    cfg
        { cfgGitHubToken =
            cliToken <|> cfgGitHubToken cfg
        , cfgAgentServer =
            fromMaybe (cfgAgentServer cfg) cliServer
        , cfgProjectId =
            cliProject <|> cfgProjectId cfg
        }

-- | Run the application
runApp :: CliOverrides -> IO ()
runApp overrides = do
    cfg <- applyOverrides overrides <$> loadConfig
    size <- getTerminalSize
    stateVar <- newTVarIO (initialState cfg size)
    withRawMode $ do
        screen <- newScreen size
        finally
            (mainLoop stateVar screen cfg)
            (flushScreen screen)

-- | Main application loop
mainLoop :: TVar AppState -> Screen -> Config -> IO ()
mainLoop stateVar screen cfg = do
    -- Set up HTTP manager
    manager <- newManager tlsManagerSettings
    let agentClient = newAgentClient manager (cfgAgentServer cfg)

    -- Set up GitHub client
    mGhClient <- case cfgGitHubToken cfg of
        Just token -> Just <$> newGitHubClient token
        Nothing -> pure Nothing

    -- Initial data load
    withAsync (refreshData stateVar mGhClient agentClient) $
        \_ -> do
            -- Start refresh timer
            withAsync
                ( refreshTimer
                    stateVar
                    mGhClient
                    agentClient
                    (cfgRefreshInterval cfg)
                )
                $ \_ -> do
                    -- Event loop
                    eventLoop
                        stateVar
                        screen
                        mGhClient
                        agentClient
                        Nothing
                        Nothing

-- | Event loop
eventLoop
    :: TVar AppState
    -> Screen
    -> Maybe GitHubClient
    -> AgentClient
    -> Maybe TerminalView
    -> Maybe TerminalConnection
    -> IO ()
eventLoop stateVar screen mGhClient agentClient mTermView mTermConn =
    do
        -- Render
        st <- readTVarIO stateVar
        renderApp screen st mTermView

        -- Wait for input (with 200ms timeout for re-render)
        hasInput <- IO.hWaitForInput IO.stdin 200
        if not hasInput
            then
                eventLoop
                    stateVar
                    screen
                    mGhClient
                    agentClient
                    mTermView
                    mTermConn
            else pure ()
        event <- readEvent
        case event of
            KeyEvent key modifier ->
                handleKey
                    stateVar
                    screen
                    mGhClient
                    agentClient
                    mTermView
                    mTermConn
                    key
                    modifier
            ResizeEvent -> do
                newSize <- getTerminalSize
                atomically $
                    modifyTVar' stateVar $ \s ->
                        s{stSize = newSize}
                eventLoop
                    stateVar
                    screen
                    mGhClient
                    agentClient
                    mTermView
                    mTermConn
            UnknownInput _ ->
                eventLoop
                    stateVar
                    screen
                    mGhClient
                    agentClient
                    mTermView
                    mTermConn

-- | Handle key events
handleKey
    :: TVar AppState
    -> Screen
    -> Maybe GitHubClient
    -> AgentClient
    -> Maybe TerminalView
    -> Maybe TerminalConnection
    -> Key
    -> Modifier
    -> IO ()
handleKey stateVar screen mGhClient agentClient mTermView mTermConn key modifier =
    do
        st <- readTVarIO stateVar
        -- If terminal is active, forward input
        if stTerminalActive st
            then case key of
                -- Ctrl-] to detach from terminal
                KeyChar ']'
                    | modCtrl modifier -> do
                        atomically $
                            modifyTVar' stateVar $ \s ->
                                s{stTerminalActive = False}
                        eventLoop
                            stateVar
                            screen
                            mGhClient
                            agentClient
                            mTermView
                            mTermConn
                _ -> do
                    -- Forward to terminal
                    case mTermConn of
                        Just conn ->
                            sendTerminalInput
                                conn
                                (keyToBytes key modifier)
                        Nothing -> pure ()
                    eventLoop
                        stateVar
                        screen
                        mGhClient
                        agentClient
                        mTermView
                        mTermConn
            else case key of
                -- Quit
                KeyChar 'q' | modifier == noMod -> do
                    -- Clean up terminal
                    mapM_ freeTerminalView mTermView
                    mapM_ closeTerminal mTermConn
                -- Navigation: h/l for columns
                KeyChar 'h' | modifier == noMod -> do
                    atomically $
                        modifyTVar' stateVar prevPage
                    continue
                KeyChar 'l' | modifier == noMod -> do
                    atomically $
                        modifyTVar' stateVar nextPage
                    continue
                -- Navigation: j/k for items
                KeyChar 'j' | modifier == noMod -> do
                    atomically $
                        modifyTVar' stateVar $ \s ->
                            let items = currentColumnItems s
                                maxIdx =
                                    max 0 (length items - 1)
                            in  s
                                    { stSelectedIndex =
                                        min
                                            (stSelectedIndex s + 1)
                                            maxIdx
                                    }
                    continue
                KeyChar 'k' | modifier == noMod -> do
                    atomically $
                        modifyTVar' stateVar $ \s ->
                            s
                                { stSelectedIndex =
                                    max
                                        0
                                        (stSelectedIndex s - 1)
                                }
                    continue
                -- Expand/collapse item
                KeyEnter -> do
                    atomically $
                        modifyTVar' stateVar $ \s ->
                            let items = currentColumnItems s
                                mItem =
                                    safeIndex
                                        (stSelectedIndex s)
                                        items
                            in  case mItem of
                                    Just item ->
                                        if stExpandedItem s
                                            == Just
                                                (itemId item)
                                            then
                                                s
                                                    { stExpandedItem =
                                                        Nothing
                                                    }
                                            else
                                                s
                                                    { stExpandedItem =
                                                        Just
                                                            (itemId item)
                                                    }
                                    Nothing -> s
                    continue
                -- Move item
                KeyChar 'm' | modifier == noMod -> do
                    handleMoveItem
                        stateVar
                        mGhClient
                    continue
                -- Agent: launch/attach
                KeyChar 'a' | modifier == noMod -> do
                    (tv, tc) <-
                        handleAgentAction
                            stateVar
                            agentClient
                            mTermView
                            mTermConn
                    eventLoop
                        stateVar
                        screen
                        mGhClient
                        agentClient
                        tv
                        tc
                -- Settings
                KeyChar 's' | modifier == noMod -> do
                    atomically $
                        modifyTVar' stateVar $ \s ->
                            s{stPage = SettingsPage}
                    continue
                -- Refresh
                KeyChar 'r' | modifier == noMod -> do
                    void $
                        async $
                            refreshData
                                stateVar
                                mGhClient
                                agentClient
                    continue
                _ -> continue
  where
    continue =
        eventLoop
            stateVar
            screen
            mGhClient
            agentClient
            mTermView
            mTermConn

-- | Move item to next/prev status
handleMoveItem
    :: TVar AppState -> Maybe GitHubClient -> IO ()
handleMoveItem stateVar mGhClient = do
    st <- readTVarIO stateVar
    let items = currentColumnItems st
        mItem = safeIndex (stSelectedIndex st) items
    case (mItem, mGhClient) of
        (Just item, Just ghClient) -> do
            let currentStatus = itemStatus item
                targetStatus = case currentStatus of
                    Just Backlog -> Just WIP
                    Just WIP -> Just Done
                    Just Done -> Just Backlog
                    Nothing -> Just Backlog
            case targetStatus of
                Just target -> do
                    let projId =
                            cfgProjectId (stConfig st)
                    case projId of
                        Just pid ->
                            case Map.lookup
                                pid
                                (stStatusFields st) of
                                Just sf ->
                                    moveItem
                                        stateVar
                                        ghClient
                                        pid
                                        item
                                        sf
                                        target
                                Nothing -> addToast stateVar Error "No status field"
                        Nothing -> addToast stateVar Error "No project selected"
                Nothing -> pure ()
        _ -> pure ()

-- | Execute item move
moveItem
    :: TVar AppState
    -> GitHubClient
    -> T.Text
    -> ProjectItem
    -> StatusField
    -> KanbanStatus
    -> IO ()
moveItem stateVar ghClient projId item sf target = do
    let targetText = statusToText target
        mOption =
            filter
                (\o -> soName o == targetText)
                (sfOptions sf)
    case mOption of
        (opt : _) -> do
            -- Optimistic update
            atomically $
                modifyTVar' stateVar $ \s ->
                    let pid = projId
                        items =
                            Map.findWithDefault
                                []
                                pid
                                (stItems s)
                        items' =
                            map
                                ( \i ->
                                    if itemId i == itemId item
                                        then
                                            i
                                                { itemStatus =
                                                    Just target
                                                }
                                        else i
                                )
                                items
                    in  s
                            { stItems =
                                Map.insert
                                    pid
                                    items'
                                    (stItems s)
                            , stSelectedIndex = 0
                            }
            -- API call
            result <-
                updateItemStatus
                    ghClient
                    projId
                    (itemId item)
                    (sfFieldId sf)
                    (soId opt)
            case result of
                Right () ->
                    addToast
                        stateVar
                        Info
                        ( "Moved to "
                            <> statusToText target
                        )
                Left e -> do
                    addToast stateVar Error e
                    -- Rollback
                    atomically $
                        modifyTVar' stateVar $ \s ->
                            let pid = projId
                                items =
                                    Map.findWithDefault
                                        []
                                        pid
                                        (stItems s)
                                items' =
                                    map
                                        ( \i ->
                                            if itemId i
                                                == itemId item
                                                then
                                                    i
                                                        { itemStatus =
                                                            itemStatus
                                                                item
                                                        }
                                                else i
                                        )
                                        items
                            in  s
                                    { stItems =
                                        Map.insert
                                            pid
                                            items'
                                            (stItems s)
                                    }
        [] ->
            addToast stateVar Error $
                "No option for " <> targetText

-- | Handle agent launch/attach
handleAgentAction
    :: TVar AppState
    -> AgentClient
    -> Maybe TerminalView
    -> Maybe TerminalConnection
    -> IO (Maybe TerminalView, Maybe TerminalConnection)
handleAgentAction stateVar agentClient mTermView mTermConn =
    do
        st <- readTVarIO stateVar
        let items = currentColumnItems st
            mItem = safeIndex (stSelectedIndex st) items
        case mItem of
            Just item
                | Just owner <- itemRepoOwner item
                , Just repo <- itemRepoName item
                , Just issue <- itemNumber item -> do
                    let sid =
                            repo
                                <> "-"
                                <> T.pack (show issue)
                    -- Check if session exists
                    case Map.lookup sid (stSessions st) of
                        Just _ -> do
                            -- Attach to existing
                            attachTerminal
                                stateVar
                                agentClient
                                sid
                                mTermView
                                mTermConn
                        Nothing -> do
                            -- Launch new session
                            result <-
                                launchSession
                                    agentClient
                                    owner
                                    repo
                                    issue
                            case result of
                                Right session -> do
                                    atomically
                                        $ modifyTVar'
                                            stateVar
                                        $ \s ->
                                            s
                                                { stSessions =
                                                    Map.insert
                                                        (asId session)
                                                        session
                                                        (stSessions s)
                                                }
                                    addToast
                                        stateVar
                                        Info
                                        ( "Launched "
                                            <> asId session
                                        )
                                    attachTerminal
                                        stateVar
                                        agentClient
                                        (asId session)
                                        mTermView
                                        mTermConn
                                Left e -> do
                                    addToast stateVar Error e
                                    pure
                                        (mTermView, mTermConn)
            _ -> do
                addToast
                    stateVar
                    Error
                    "No issue selected"
                pure (mTermView, mTermConn)

-- | Attach terminal to a session
attachTerminal
    :: TVar AppState
    -> AgentClient
    -> T.Text
    -> Maybe TerminalView
    -> Maybe TerminalConnection
    -> IO (Maybe TerminalView, Maybe TerminalConnection)
attachTerminal stateVar _agentClient sessionId mTermView mTermConn =
    do
        -- Clean up old terminal
        mapM_ freeTerminalView mTermView
        mapM_ closeTerminal mTermConn
        st <- readTVarIO stateVar
        let rect = terminalRect (stSize st)
            rows = rectHeight rect - 1
            cols = rectWidth rect - 1
        -- Create terminal view
        tv <-
            newTerminalView
                rows
                cols
                (Just sessionId)
        -- Connect WebSocket
        let serverUrl =
                cfgAgentServer (stConfig st)
            host =
                T.drop 7 $
                    T.takeWhile (/= ':') $
                        T.drop 7 serverUrl
            port = 8080
        result <-
            connectTerminal
                (if T.null host then "localhost" else host)
                port
                sessionId
        case result of
            Right conn -> do
                -- Send initial resize
                sendTerminalResize conn cols rows
                atomically $
                    modifyTVar' stateVar $ \s ->
                        s
                            { stTerminalActive = True
                            , stActiveTerminal =
                                Just sessionId
                            }
                -- Start receive loop in background
                void $
                    async $
                        terminalReceiveLoop
                            stateVar
                            tv
                            conn
                pure (Just tv, Just conn)
            Left e -> do
                addToast stateVar Error $
                    "Connect failed: " <> e
                freeTerminalView tv
                pure (Nothing, Nothing)

-- | Background loop receiving terminal output
terminalReceiveLoop
    :: TVar AppState
    -> TerminalView
    -> TerminalConnection
    -> IO ()
terminalReceiveLoop stateVar tv conn = do
    mData <- receiveTerminalOutput conn
    case mData of
        Just bs -> do
            feedTerminalView tv bs
            terminalReceiveLoop stateVar tv conn
        Nothing -> do
            atomically $
                modifyTVar' stateVar $ \s ->
                    s
                        { stTerminalActive = False
                        , stActiveTerminal = Nothing
                        }

-- | Render the full application
renderApp
    :: Screen -> AppState -> Maybe TerminalView -> IO ()
renderApp screen st mTermView = do
    let header = renderHeader st
        content = case stPage st of
            SettingsPage -> renderSettings st
            _ -> renderKanban st
        statusBar = renderStatusBar st
    termBuild <- case mTermView of
        Just tv | stTerminalActive st -> do
            let rect = terminalRect (stSize st)
            renderTerminalView tv rect
        _ -> pure mempty
    renderToScreen screen $
        clearScreen
            <> header
            <> content
            <> termBuild
            <> statusBar

-- | Refresh all data
refreshData
    :: TVar AppState
    -> Maybe GitHubClient
    -> AgentClient
    -> IO ()
refreshData stateVar mGhClient agentClient = do
    atomically $
        modifyTVar' stateVar $ \s ->
            s{stLoading = True}
    -- Fetch GitHub data
    case mGhClient of
        Just ghClient -> do
            projResult <- fetchProjects ghClient
            case projResult of
                Right projs -> do
                    atomically $
                        modifyTVar' stateVar $ \s ->
                            s{stProjects = projs}
                    -- Fetch items for selected project
                    st <- readTVarIO stateVar
                    case cfgProjectId (stConfig st) of
                        Just pid -> do
                            itemResult <-
                                fetchProjectItems
                                    ghClient
                                    pid
                            case itemResult of
                                Right (items, sf) ->
                                    atomically
                                        $ modifyTVar'
                                            stateVar
                                        $ \s' ->
                                            s'
                                                { stItems =
                                                    Map.insert
                                                        pid
                                                        items
                                                        (stItems s')
                                                , stStatusFields =
                                                    Map.insert
                                                        pid
                                                        sf
                                                        (stStatusFields s')
                                                }
                                Left e ->
                                    addToast
                                        stateVar
                                        Error
                                        e
                        Nothing -> pure ()
                Left e -> addToast stateVar Error e
        Nothing -> pure ()
    -- Fetch agent data
    sessResult <- listSessions agentClient
    case sessResult of
        Right sessions ->
            atomically $
                modifyTVar' stateVar $ \s ->
                    s
                        { stSessions =
                            Map.fromList $
                                map (\sess -> (asId sess, sess)) sessions
                        }
        Left _ -> pure ()
    branchResult <- listBranches agentClient
    case branchResult of
        Right branches ->
            atomically $
                modifyTVar' stateVar $ \s ->
                    s
                        { stBranches =
                            Map.fromList $
                                map
                                    ( \b ->
                                        ( Kanbanned.Agent.Types.biName b
                                        , b
                                        )
                                    )
                                    branches
                        }
        Left _ -> pure ()
    atomically $
        modifyTVar' stateVar $ \s ->
            s{stLoading = False}

-- | Periodic refresh timer
refreshTimer
    :: TVar AppState
    -> Maybe GitHubClient
    -> AgentClient
    -> Int
    -> IO ()
refreshTimer stateVar mGhClient agentClient interval =
    forever $ do
        threadDelay (interval * 1_000_000)
        refreshData stateVar mGhClient agentClient

-- | Add a toast notification
addToast :: TVar AppState -> ToastLevel -> T.Text -> IO ()
addToast stateVar level msg =
    atomically $
        modifyTVar' stateVar $ \s ->
            s
                { stToasts =
                    Toast
                        { toastMessage = msg
                        , toastLevel = level
                        , toastTTL = 3
                        }
                        : take 4 (stToasts s)
                }

-- | Navigate to previous page
prevPage :: AppState -> AppState
prevPage s = s{stPage = prev (stPage s), stSelectedIndex = 0}
  where
    prev BacklogPage = DonePage
    prev WIPPage = BacklogPage
    prev DonePage = WIPPage
    prev SettingsPage = DonePage

-- | Navigate to next page
nextPage :: AppState -> AppState
nextPage s = s{stPage = next (stPage s), stSelectedIndex = 0}
  where
    next BacklogPage = WIPPage
    next WIPPage = DonePage
    next DonePage = BacklogPage
    next SettingsPage = BacklogPage

-- | Safe list indexing
safeIndex :: Int -> [a] -> Maybe a
safeIndex _ [] = Nothing
safeIndex 0 (x : _) = Just x
safeIndex n (_ : xs)
    | n > 0 = safeIndex (n - 1) xs
    | otherwise = Nothing

-- | Convert key event to bytes for terminal
keyToBytes :: Key -> Modifier -> ByteString
keyToBytes key _modifier = case key of
    KeyChar c -> encodeChar c
    KeyEnter -> "\r"
    KeyTab -> "\t"
    KeyBackspace -> "\x7f"
    KeyEscape -> "\x1b"
    KeyUp -> "\x1b[A"
    KeyDown -> "\x1b[B"
    KeyRight -> "\x1b[C"
    KeyLeft -> "\x1b[D"
    KeyHome -> "\x1b[H"
    KeyEnd -> "\x1b[F"
    KeyPageUp -> "\x1b[5~"
    KeyPageDown -> "\x1b[6~"
    KeyDelete -> "\x1b[3~"
    KeyInsert -> "\x1b[2~"
    KeyF n -> "\x1b[" <> encodeChar (toEnum (n + 48)) <> "~"
  where
    encodeChar :: Char -> ByteString
    encodeChar c = TE.encodeUtf8 $ T.singleton c
