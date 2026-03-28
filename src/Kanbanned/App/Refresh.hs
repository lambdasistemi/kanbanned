{- |
Module      : Kanbanned.App.Refresh
Description : Background data refresh and auto-attach terminals
-}
module Kanbanned.App.Refresh
    ( refreshData
    , refreshLoop
    ) where

import Brick.BChan qualified
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, forever, void)
import Data.IORef (modifyIORef', readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Kanbanned.Agent.Rest (listBranches, listSessions)
import Kanbanned.Agent.Types qualified
import Kanbanned.Agent.WebSocket
    ( TerminalConnection
    , connectTerminal
    , receiveTerminalOutput
    , sendTerminalResize
    )
import Kanbanned.App.Env
    ( Env (..)
    , TerminalState (..)
    , sendEvent
    , updateState
    )
import Kanbanned.Config (Config (..))
import Kanbanned.GitHub.GraphQL
    ( fetchProjectItems
    , fetchProjects
    )
import Kanbanned.State
    ( AppEvent (..)
    , AppState (..)
    , ItemView (..)
    )
import Kanbanned.UI.Terminal
    ( TerminalView
    , feedTerminalView
    , freeTerminalView
    , getTerminalImage
    , newTerminalView
    )
import System.Timeout (timeout)

-- | Periodic refresh loop
refreshLoop :: Env -> Config -> IO ()
refreshLoop env cfg = do
    refreshData env
    forever $ do
        threadDelay (cfgRefreshInterval cfg * 1_000_000)
        refreshData env

-- | Fetch all data and auto-attach new sessions
refreshData :: Env -> IO ()
refreshData env = do
    updateState env $ \s -> s{stLoading = True}
    refreshGitHub env
    refreshAgentSessions env
    refreshBranches env
    updateState env $ \s ->
        s{stLoading = False, stToast = Nothing}

refreshGitHub :: Env -> IO ()
refreshGitHub env =
    case envGhClient env of
        Just ghClient -> do
            projResult <- fetchProjects ghClient
            case projResult of
                Right projs -> do
                    updateState env $ \s ->
                        s{stProjects = projs}
                    case cfgProjectId (envConfig env) of
                        Just pid -> do
                            itemResult <-
                                fetchProjectItems
                                    ghClient
                                    pid
                            case itemResult of
                                Right (items, sf) ->
                                    updateState env $ \s ->
                                        s
                                            { stItems =
                                                Map.insert
                                                    pid
                                                    items
                                                    (stItems s)
                                            , stStatusFields =
                                                Map.insert
                                                    pid
                                                    sf
                                                    (stStatusFields s)
                                            }
                                Left e ->
                                    sendEvent
                                        env
                                        (ErrorEvent e)
                        Nothing -> pure ()
                Left e -> sendEvent env (ErrorEvent e)
        Nothing -> pure ()

refreshAgentSessions :: Env -> IO ()
refreshAgentSessions env = do
    sessResult <- listSessions (envAgentClient env)
    case sessResult of
        Right sessions -> do
            let sessMap =
                    Map.fromList $
                        map
                            ( \sess ->
                                ( Kanbanned.Agent.Types.asId sess
                                , sess
                                )
                            )
                            sessions
            updateState env $ \s ->
                s{stSessions = sessMap}
            -- Auto-attach terminals for new sessions
            existing <- readIORef (envTerminals env)
            let newSids =
                    filter
                        (`Map.notMember` existing)
                        (Map.keys sessMap)
            forM_ newSids $ \sid ->
                void $ forkIO $ autoAttach env sid
        Left e -> sendEvent env (ErrorEvent e)

refreshBranches :: Env -> IO ()
refreshBranches env = do
    branchResult <- listBranches (envAgentClient env)
    case branchResult of
        Right branches ->
            updateState env $ \s ->
                s
                    { stBranches =
                        Map.fromList $
                            map
                                ( \b ->
                                    ( Kanbanned.Agent.Types.biName
                                        b
                                    , b
                                    )
                                )
                                branches
                    }
        Left e -> sendEvent env (ErrorEvent e)

-- | Auto-attach to a session's terminal WebSocket
autoAttach :: Env -> T.Text -> IO ()
autoAttach env sessionId = do
    let (host, port) =
            parseHostPort
                (cfgAgentServer (envConfig env))
    mResult <-
        timeout 10_000_000 $
            connectTerminal host port sessionId
    case mResult of
        Just (Right conn) -> do
            tv <- newTerminalView 24 80 (Just sessionId)
            sendTerminalResize conn 80 24
            modifyIORef' (envTerminals env) $
                Map.insert
                    sessionId
                    TerminalState{tsView = tv, tsConn = conn}
            updateState env $ \s ->
                s
                    { stItemViews =
                        Map.insertWith
                            (\_ old -> old)
                            sessionId
                            ShowTerminal
                            (stItemViews s)
                    }
            -- Start receive loop
            terminalReceiveLoop env sessionId tv conn
        Just (Left _) -> pure ()
        Nothing -> pure ()

-- | Background loop receiving terminal output
terminalReceiveLoop
    :: Env -> T.Text -> TerminalView -> TerminalConnection -> IO ()
terminalReceiveLoop env sid tv conn = go
  where
    go = do
        mData <- receiveTerminalOutput conn
        case mData of
            Just bs -> do
                feedTerminalView tv bs
                img <- getTerminalImage tv
                void $
                    Brick.BChan.writeBChanNonBlocking
                        (envChan env)
                        ( StateUpdate $ \s ->
                            s
                                { stTerminalImages =
                                    Map.insert
                                        sid
                                        img
                                        (stTerminalImages s)
                                }
                        )
                go
            Nothing -> do
                -- Connection closed — clean up
                freeTerminalView tv
                modifyIORef' (envTerminals env) $
                    Map.delete sid
                updateState env $ \s ->
                    s
                        { stTerminalImages =
                            Map.delete
                                sid
                                (stTerminalImages s)
                        }

parseHostPort :: T.Text -> (T.Text, Int)
parseHostPort url =
    let stripped =
            T.dropWhile (== '/') $
                T.drop 1 $
                    T.dropWhile (/= ':') url
    in  case T.splitOn ":" stripped of
            [h, p] ->
                ( resolveHost h
                , maybe 8080 fst (readInt p)
                )
            [h] -> (resolveHost h, 8080)
            _ -> ("127.0.0.1", 8080)
  where
    resolveHost h
        | h == "localhost" = "127.0.0.1"
        | otherwise = h
    readInt :: T.Text -> Maybe (Int, String)
    readInt t = case reads (T.unpack t) of
        [(n, "")] -> Just (n, "")
        _ -> Nothing
