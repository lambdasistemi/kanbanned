{- |
Module      : Kanbanned.App.Refresh
Description : Background data refresh from GitHub and agent-daemon
-}
module Kanbanned.App.Refresh
    ( refreshData
    , refreshLoop
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Map.Strict qualified as Map
import Kanbanned.Agent.Rest (listBranches, listSessions)
import Kanbanned.Agent.Types qualified
import Kanbanned.App.Env (Env (..), sendEvent, updateState)
import Kanbanned.Config (Config (..))
import Kanbanned.GitHub.GraphQL
    ( fetchProjectItems
    , fetchProjects
    )
import Kanbanned.State (AppEvent (..), AppState (..))

-- | Periodic refresh loop
refreshLoop :: Env -> Config -> IO ()
refreshLoop env cfg = do
    refreshData env
    forever $ do
        threadDelay (cfgRefreshInterval cfg * 1_000_000)
        refreshData env

-- | Fetch all data from GitHub and agent-daemon
refreshData :: Env -> IO ()
refreshData env = do
    updateState env $ \s -> s{stLoading = True}
    -- GitHub data
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
                                fetchProjectItems ghClient pid
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
                                    sendEvent env (ErrorEvent e)
                        Nothing -> pure ()
                Left e -> sendEvent env (ErrorEvent e)
        Nothing -> pure ()
    -- Agent-daemon data
    sessResult <- listSessions (envAgentClient env)
    case sessResult of
        Right sessions ->
            updateState env $ \s ->
                s
                    { stSessions =
                        Map.fromList $
                            map
                                (\sess -> (Kanbanned.Agent.Types.asId sess, sess))
                                sessions
                    }
        Left e -> sendEvent env (ErrorEvent e)
    branchResult <- listBranches (envAgentClient env)
    case branchResult of
        Right branches ->
            updateState env $ \s ->
                s
                    { stBranches =
                        Map.fromList $
                            map
                                (\b -> (Kanbanned.Agent.Types.biName b, b))
                                branches
                    }
        Left e -> sendEvent env (ErrorEvent e)
    updateState env $ \s ->
        s{stLoading = False, stToast = Nothing}
