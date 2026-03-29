{- |
Module      : Kanbanned.App.Env
Description : Shared environment for async operations
-}
module Kanbanned.App.Env
    ( Env (..)
    , TerminalState (..)
    , sendEvent
    , updateState
    ) where

import Brick.BChan (BChan, writeBChan)
import Control.Concurrent.Async (Async)
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Kanbanned.Agent.Rest (AgentClient)
import Kanbanned.Agent.WebSocket (TerminalConnection)
import Kanbanned.Config (Config)
import Kanbanned.GitHub.GraphQL (GitHubClient)
import Kanbanned.State (AppEvent (..), AppState)
import Kanbanned.UI.Terminal (TerminalView)

-- | Per-session terminal state
data TerminalState = TerminalState
    { tsView :: !TerminalView
    , tsConn :: !TerminalConnection
    , tsReceiveThread :: !(Async ())
    -- ^ Background receive loop thread
    }

-- | Shared environment for background threads
data Env = Env
    { envChan :: !(BChan AppEvent)
    , envConfig :: !Config
    , envGhClient :: !(Maybe GitHubClient)
    , envAgentClient :: !AgentClient
    , envTerminals :: !(IORef (Map Text TerminalState))
    -- ^ All active terminal connections, keyed by session ID
    }

-- | Send an event to the brick event loop
sendEvent :: Env -> AppEvent -> IO ()
sendEvent = writeBChan . envChan

-- | Send a state update to the brick event loop
updateState :: Env -> (AppState -> AppState) -> IO ()
updateState env = sendEvent env . StateUpdate
