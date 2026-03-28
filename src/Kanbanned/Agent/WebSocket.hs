{- |
Module      : Kanbanned.Agent.WebSocket
Description : WebSocket client for agent-daemon terminal I/O
-}
module Kanbanned.Agent.WebSocket
    ( TerminalConnection (..)
    , connectTerminal
    , sendTerminalInput
    , sendTerminalResize
    , receiveTerminalOutput
    , closeTerminal
    ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, cancel)
import Control.Concurrent.STM
    ( TVar
    , atomically
    , newTVarIO
    , readTVar
    , readTVarIO
    , retry
    , writeTVar
    )
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Network.WebSockets qualified as WS

-- | A connection to an agent terminal
data TerminalConnection = TerminalConnection
    { tcConnection :: !WS.Connection
    , tcSessionId :: !Text
    , tcAlive :: !(TVar Bool)
    }

-- | Connect to an agent terminal WebSocket
connectTerminal
    :: Text
    -- ^ Host (e.g. "localhost")
    -> Int
    -- ^ Port
    -> Text
    -- ^ Session ID
    -> IO (Either Text TerminalConnection)
connectTerminal host port sessionId = do
    let path =
            "/sessions/"
                <> T.unpack sessionId
                <> "/terminal"
    alive <- newTVarIO True
    connVar <- newTVarIO (Nothing :: Maybe WS.Connection)
    errVar <- newTVarIO (Nothing :: Maybe Text)
    -- runClient blocks until callback returns, so we
    -- run it in a thread and wait for the connection
    thread <-
        async $
            WS.runClient
                (T.unpack host)
                port
                path
                ( \conn -> do
                    atomically $ writeTVar connVar (Just conn)
                    -- Block until connection is closed
                    atomically $ do
                        a <- readTVar alive
                        when a retry
                )
                `catch` \(e :: SomeException) ->
                    atomically $ do
                        writeTVar alive False
                        writeTVar errVar (Just $ T.pack $ show e)
    -- Wait for connection to be established (up to 5s)
    mConn <- waitForConn connVar alive 50
    case mConn of
        Just conn ->
            pure $
                Right
                    TerminalConnection
                        { tcConnection = conn
                        , tcSessionId = sessionId
                        , tcAlive = alive
                        }
        Nothing -> do
            atomically $ writeTVar alive False
            cancel thread
            mErr <- readTVarIO errVar
            pure $ Left $ fromMaybe "WebSocket timeout" mErr

-- | Send raw terminal input
sendTerminalInput
    :: TerminalConnection -> ByteString -> IO ()
sendTerminalInput TerminalConnection{..} input = do
    alive <- readTVarIO tcAlive
    when alive $
        WS.sendBinaryData tcConnection input
            `catch` \(_ :: SomeException) ->
                atomically (writeTVar tcAlive False)

-- | Send terminal resize command
sendTerminalResize
    :: TerminalConnection -> Int -> Int -> IO ()
sendTerminalResize conn cols rows =
    sendTerminalInput conn $
        BS.singleton 0x01
            <> BS8.pack (show cols <> ";" <> show rows)

-- | Receive terminal output (blocking)
receiveTerminalOutput
    :: TerminalConnection -> IO (Maybe ByteString)
receiveTerminalOutput TerminalConnection{..} = do
    alive <- readTVarIO tcAlive
    if alive
        then
            ( do
                msg <- WS.receiveData tcConnection
                pure $ Just msg
            )
                `catch` \(_ :: SomeException) -> do
                    atomically $ writeTVar tcAlive False
                    pure Nothing
        else pure Nothing

-- | Close the terminal connection
closeTerminal :: TerminalConnection -> IO ()
closeTerminal TerminalConnection{..} = do
    atomically $ writeTVar tcAlive False
    WS.sendClose tcConnection ("bye" :: ByteString)
        `catch` \(_ :: SomeException) -> pure ()

-- | Poll for connection to be established
waitForConn
    :: TVar (Maybe WS.Connection)
    -> TVar Bool
    -> Int
    -> IO (Maybe WS.Connection)
waitForConn connVar alive remaining
    | remaining <= 0 = pure Nothing
    | otherwise = do
        mConn <- readTVarIO connVar
        case mConn of
            Just conn -> pure (Just conn)
            Nothing -> do
                a <- readTVarIO alive
                if a
                    then do
                        threadDelay 100_000
                        waitForConn
                            connVar
                            alive
                            (remaining - 1)
                    else pure Nothing
