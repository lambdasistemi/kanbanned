{- |
Module      : Kanbanned.App
Description : Brick application entry point
-}
module Kanbanned.App
    ( runApp
    , CliOverrides (..)
    , noOverrides
    ) where

import Brick (App (..), customMain)
import Brick.BChan (newBChan)
import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Async (cancel)
import Control.Monad (void)
import Data.IORef (newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)
import Kanbanned.Agent.Rest (newAgentClient)
import Kanbanned.Agent.WebSocket (closeTerminal)
import Kanbanned.App.Attrs (theAttrMap)
import Kanbanned.App.Env (Env (..), TerminalState (..))
import Kanbanned.App.Event (handleEvent)
import Kanbanned.App.Refresh (refreshLoop)
import Kanbanned.Config (Config (..), loadConfig)
import Kanbanned.GitHub.GraphQL (newGitHubClient)
import Kanbanned.State (AppEvent, AppState, Name, initialState)
import Kanbanned.UI.Draw (drawUI)
import Kanbanned.UI.Terminal (freeTerminalView)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (setEnv)

------------------------------------------------------------------------
-- CLI overrides
------------------------------------------------------------------------

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

------------------------------------------------------------------------
-- Entry point
------------------------------------------------------------------------

-- | Run the application
runApp :: CliOverrides -> IO ()
runApp overrides = do
    cfg <- applyOverrides overrides <$> loadConfig
    chan <- newBChan 10
    manager <- newManager tlsManagerSettings
    terminalsRef <- newIORef Map.empty
    mGhClient <- case cfgGitHubToken cfg of
        Just token -> Just <$> newGitHubClient token
        Nothing -> pure Nothing
    let env =
            Env
                { envChan = chan
                , envConfig = cfg
                , envGhClient = mGhClient
                , envAgentClient =
                    newAgentClient manager (cfgAgentServer cfg)
                , envTerminals = terminalsRef
                }
    void $ forkIO $ refreshLoop env cfg
    setEnv "TERM" "xterm-256color"
    let buildVty = do
            v <- mkVty V.defaultConfig
            V.setMode (V.outputIface v) V.Mouse True
            pure v
    initialVty <- buildVty
    void $
        customMain
            initialVty
            buildVty
            (Just chan)
            (brickApp env)
            (initialState cfg)
    -- Cleanup: close connections (unblocks receive loops),
    -- cancel threads, then free views
    terminals <- readIORef terminalsRef
    mapM_ (closeTerminal . tsConn) (Map.elems terminals)
    mapM_ (cancel . tsReceiveThread) (Map.elems terminals)
    mapM_ (freeTerminalView . tsView) (Map.elems terminals)

------------------------------------------------------------------------
-- Brick app definition
------------------------------------------------------------------------

brickApp :: Env -> App AppState AppEvent Name
brickApp env =
    App
        { appDraw = drawUI
        , appChooseCursor = \_ _ -> Nothing
        , appHandleEvent = handleEvent env
        , appStartEvent = pure ()
        , appAttrMap = const theAttrMap
        }
