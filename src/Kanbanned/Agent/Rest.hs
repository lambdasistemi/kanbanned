{- |
Module      : Kanbanned.Agent.Rest
Description : Agent-daemon REST API client
-}
module Kanbanned.Agent.Rest
    ( AgentClient (..)
    , newAgentClient
    , listSessions
    , launchSession
    , stopSession
    , listWorktrees
    , listBranches
    ) where

import Data.Aeson (FromJSON, eitherDecode, encode, object, (.=))
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import Kanbanned.Agent.Types
    ( AgentSession
    , BranchInfo
    , WorktreeInfo
    )
import Network.HTTP.Client
    ( Manager
    , Request (..)
    , RequestBody (..)
    , Response
    , httpLbs
    , parseRequest
    , responseBody
    , responseStatus
    )
import Network.HTTP.Types
    ( hContentType
    , statusCode
    )

-- | Agent-daemon REST client
data AgentClient = AgentClient
    { acManager :: !Manager
    , acBaseUrl :: !Text
    }

-- | Create a new agent client
newAgentClient :: Manager -> Text -> AgentClient
newAgentClient manager baseUrl =
    AgentClient
        { acManager = manager
        , acBaseUrl = baseUrl
        }

-- | List all active sessions
listSessions
    :: AgentClient -> IO (Either Text [AgentSession])
listSessions client = do
    result <- agentGet client "/sessions"
    pure $ result >>= eitherDecode'

-- | Launch a new session for an issue
launchSession
    :: AgentClient
    -> Text
    -> Text
    -> Int
    -> IO (Either Text AgentSession)
launchSession client owner name issue = do
    result <-
        agentPost
            client
            "/sessions"
            ( encode $
                object
                    [ "repo"
                        .= object
                            [ "owner" .= owner
                            , "name" .= name
                            ]
                    , "issue" .= issue
                    ]
            )
    pure $ result >>= eitherDecode'

-- | Stop a session
stopSession :: AgentClient -> Text -> IO (Either Text ())
stopSession client sessionId = do
    result <-
        agentDelete client $
            "/sessions/" <> T.unpack sessionId
    pure $ result >> Right ()

-- | List all worktrees
listWorktrees
    :: AgentClient -> IO (Either Text [WorktreeInfo])
listWorktrees client = do
    result <- agentGet client "/worktrees"
    pure $ result >>= eitherDecode'

-- | List all branches
listBranches
    :: AgentClient -> IO (Either Text [BranchInfo])
listBranches client = do
    result <- agentGet client "/branches"
    pure $ result >>= eitherDecode'

-- HTTP helpers

-- | Check response status and return body or error
requireSuccess
    :: Response LBS.ByteString
    -> Either Text LBS.ByteString
requireSuccess resp =
    let code = statusCode (responseStatus resp)
    in  if code >= 200 && code < 300
            then Right $ responseBody resp
            else
                Left $
                    "HTTP "
                        <> T.pack (show code)
                        <> ": "
                        <> T.pack
                            ( show $
                                LBS.take 200 (responseBody resp)
                            )

agentGet
    :: AgentClient -> String -> IO (Either Text LBS.ByteString)
agentGet AgentClient{..} path = do
    req <- parseRequest $ T.unpack acBaseUrl <> path
    resp <- httpLbs req acManager
    pure $ requireSuccess resp

agentPost
    :: AgentClient
    -> String
    -> LBS.ByteString
    -> IO (Either Text LBS.ByteString)
agentPost AgentClient{..} path body = do
    req0 <- parseRequest $ T.unpack acBaseUrl <> path
    let req =
            req0
                { method = "POST"
                , requestHeaders =
                    [(hContentType, "application/json")]
                , requestBody = RequestBodyLBS body
                }
    resp <- httpLbs req acManager
    pure $ requireSuccess resp

agentDelete
    :: AgentClient -> String -> IO (Either Text LBS.ByteString)
agentDelete AgentClient{..} path = do
    req0 <- parseRequest $ T.unpack acBaseUrl <> path
    let req = req0{method = "DELETE"}
    resp <- httpLbs req acManager
    pure $ requireSuccess resp

eitherDecode'
    :: (FromJSON a)
    => LBS.ByteString
    -> Either Text a
eitherDecode' bs = case eitherDecode bs of
    Left e -> Left $ T.pack e
    Right a -> Right a
