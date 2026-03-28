{- |
Module      : Kanbanned.App.Env
Description : Shared environment for async operations
-}
module Kanbanned.App.Env
    ( Env (..)
    , sendEvent
    , updateState
    ) where

import Brick.BChan (BChan, writeBChan)
import Data.IORef (IORef)
import Kanbanned.Agent.Rest (AgentClient)
import Kanbanned.Agent.WebSocket (TerminalConnection)
import Kanbanned.Config (Config)
import Kanbanned.GitHub.GraphQL (GitHubClient)
import Kanbanned.State (AppEvent (..), AppState)
import Kanbanned.UI.Terminal (TerminalView)

-- | Shared environment for background threads
data Env = Env
    { envChan :: !(BChan AppEvent)
    , envConfig :: !Config
    , envGhClient :: !(Maybe GitHubClient)
    , envAgentClient :: !AgentClient
    , envTermView :: !(IORef (Maybe TerminalView))
    , envTermConn :: !(IORef (Maybe TerminalConnection))
    }

-- | Send an event to the brick event loop
sendEvent :: Env -> AppEvent -> IO ()
sendEvent = writeBChan . envChan

-- | Send a state update to the brick event loop
updateState :: Env -> (AppState -> AppState) -> IO ()
updateState env = sendEvent env . StateUpdate
