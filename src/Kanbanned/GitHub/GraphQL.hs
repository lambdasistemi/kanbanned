{- |
Module      : Kanbanned.GitHub.GraphQL
Description : GitHub GraphQL client for Projects v2
-}
module Kanbanned.GitHub.GraphQL
    ( GitHubClient (..)
    , newGitHubClient
    , fetchProjects
    , fetchProjectItems
    , updateItemStatus
    , addDraftItem
    , deleteProjectItem
    ) where

import Control.Applicative ((<|>))
import Data.Aeson
    ( Value (..)
    , eitherDecode
    , encode
    , object
    , withObject
    , (.:)
    , (.:?)
    , (.=)
    )
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Kanbanned.GitHub.Types
    ( ItemType (..)
    , KanbanStatus
    , Project (..)
    , ProjectItem (..)
    , StatusField (..)
    , textToStatus
    )
import Network.HTTP.Client
    ( Manager
    , Request (..)
    , RequestBody (..)
    , httpLbs
    , parseRequest
    , responseBody
    )
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types (hAuthorization, hContentType)

-- | GitHub GraphQL client
data GitHubClient = GitHubClient
    { ghManager :: !Manager
    , ghToken :: !Text
    }

-- | Create a new GitHub client
newGitHubClient :: Text -> IO GitHubClient
newGitHubClient token = do
    manager <- newTlsManager
    pure GitHubClient{ghManager = manager, ghToken = token}

-- | Execute a GraphQL query
graphql
    :: GitHubClient -> Text -> Value -> IO (Either Text Value)
graphql GitHubClient{..} query variables = do
    req0 <- parseRequest "https://api.github.com/graphql"
    let body =
            encode $
                object
                    [ "query" .= query
                    , "variables" .= variables
                    ]
        req =
            req0
                { method = "POST"
                , requestHeaders =
                    [
                        ( hAuthorization
                        , "Bearer " <> TE.encodeUtf8 ghToken
                        )
                    , (hContentType, "application/json")
                    , ("User-Agent", "kanbanned/0.1")
                    ]
                , requestBody = RequestBodyLBS body
                }
    resp <- httpLbs req ghManager
    case eitherDecode (responseBody resp) of
        Left e -> pure $ Left $ T.pack e
        Right val -> case extractData val of
            Just d -> pure $ Right d
            Nothing -> case extractErrors val of
                Just e -> pure $ Left e
                Nothing ->
                    pure $ Left "Unknown GraphQL error"

-- | Extract "data" field from response
extractData :: Value -> Maybe Value
extractData = parseMaybe $ withObject "resp" (.: "data")

-- | Extract error messages from response
extractErrors :: Value -> Maybe Text
extractErrors = parseMaybe parser
  where
    parser = withObject "resp" $ \o -> do
        errs <- o .: "errors"
        msgs <- mapM (withObject "err" (.: "message")) errs
        pure $ T.intercalate "; " (msgs :: [Text])

-- | Fetch all projects for the authenticated user
fetchProjects :: GitHubClient -> IO (Either Text [Project])
fetchProjects client = do
    result <- graphql client projectsQuery (object [])
    pure $ result >>= parseProjects

-- | Fetch items for a project by node ID
fetchProjectItems
    :: GitHubClient
    -> Text
    -> IO (Either Text ([ProjectItem], StatusField))
fetchProjectItems client nodeId = do
    result <-
        graphql
            client
            itemsQuery
            (object ["nodeId" .= nodeId])
    pure $ result >>= parseItems

-- | Update an item's status on a project board
updateItemStatus
    :: GitHubClient
    -> Text
    -> Text
    -> Text
    -> Text
    -> IO (Either Text ())
updateItemStatus client projId itemId fieldId optionId =
    do
        result <-
            graphql
                client
                updateStatusMutation
                ( object
                    [ "projectId" .= projId
                    , "itemId" .= itemId
                    , "fieldId" .= fieldId
                    , "optionId" .= optionId
                    ]
                )
        pure $ result >> Right ()

-- | Add a draft issue to a project
addDraftItem
    :: GitHubClient -> Text -> Text -> IO (Either Text Text)
addDraftItem client projId title = do
    result <-
        graphql
            client
            addDraftMutation
            ( object
                [ "projectId" .= projId
                , "title" .= title
                ]
            )
    pure $ result >>= parseDraftId

-- | Delete an item from a project
deleteProjectItem
    :: GitHubClient -> Text -> Text -> IO (Either Text ())
deleteProjectItem client projId itemId = do
    result <-
        graphql
            client
            deleteMutation
            ( object
                [ "projectId" .= projId
                , "itemId" .= itemId
                ]
            )
    pure $ result >> Right ()

-- Queries and mutations

projectsQuery :: Text
projectsQuery =
    "query { \
    \  viewer { \
    \    projectsV2(first: 20) { \
    \      nodes { \
    \        id \
    \        title \
    \        number \
    \        items { totalCount } \
    \      } \
    \    } \
    \  } \
    \}"

itemsQuery :: Text
itemsQuery =
    "query($nodeId: ID!) { \
    \  node(id: $nodeId) { \
    \    ... on ProjectV2 { \
    \      fields(first: 20) { \
    \        nodes { \
    \          ... on ProjectV2SingleSelectField { \
    \            id \
    \            name \
    \            options { id name } \
    \          } \
    \        } \
    \      } \
    \      items(first: 100) { \
    \        nodes { \
    \          id \
    \          fieldValues(first: 10) { \
    \            nodes { \
    \              ... on ProjectV2ItemFieldSingleSelectValue \
    \              { \
    \                name \
    \                field { \
    \                  ... on ProjectV2SingleSelectField \
    \                  { name } \
    \                } \
    \              } \
    \            } \
    \          } \
    \          content { \
    \            ... on Issue { \
    \              title number body \
    \              labels(first: 10) { nodes { name } } \
    \              repository { \
    \                name owner { login } \
    \              } \
    \            } \
    \            ... on PullRequest { \
    \              title number body \
    \              labels(first: 10) { nodes { name } } \
    \              repository { \
    \                name owner { login } \
    \              } \
    \            } \
    \            ... on DraftIssue { \
    \              title body \
    \            } \
    \          } \
    \        } \
    \      } \
    \    } \
    \  } \
    \}"

updateStatusMutation :: Text
updateStatusMutation =
    "mutation($projectId: ID!, $itemId: ID!, \
    \$fieldId: ID!, $optionId: String!) { \
    \  updateProjectV2ItemFieldValue(input: { \
    \    projectId: $projectId \
    \    itemId: $itemId \
    \    fieldId: $fieldId \
    \    value: { \
    \      singleSelectOptionId: $optionId \
    \    } \
    \  }) { projectV2Item { id } } \
    \}"

addDraftMutation :: Text
addDraftMutation =
    "mutation($projectId: ID!, $title: String!) { \
    \  addProjectV2DraftIssue(input: { \
    \    projectId: $projectId \
    \    title: $title \
    \  }) { projectV2Item { id } } \
    \}"

deleteMutation :: Text
deleteMutation =
    "mutation($projectId: ID!, $itemId: ID!) { \
    \  deleteProjectV2Item(input: { \
    \    projectId: $projectId \
    \    itemId: $itemId \
    \  }) { deletedItemId } \
    \}"

-- Parsing helpers

parseProjects :: Value -> Either Text [Project]
parseProjects val =
    case parseMaybe parser val of
        Just ps -> Right ps
        Nothing -> Left "Failed to parse projects"
  where
    parser :: Value -> Parser [Project]
    parser = withObject "data" $ \o -> do
        viewer <- o .: "viewer"
        projs <- viewer .: "projectsV2"
        nodes <- projs .: "nodes"
        mapM parseProject (nodes :: [Value])

    parseProject :: Value -> Parser Project
    parseProject = withObject "project" $ \o -> do
        projectId <- o .: "id"
        projectTitle <- o .: "title"
        projectNumber <- o .: "number"
        items <- o .: "items"
        projectItemCount <- items .: "totalCount"
        pure Project{..}

parseItems
    :: Value -> Either Text ([ProjectItem], StatusField)
parseItems val =
    case parseMaybe parser val of
        Just r -> Right r
        Nothing -> Left "Failed to parse project items"
  where
    parser :: Value -> Parser ([ProjectItem], StatusField)
    parser = withObject "data" $ \o -> do
        node <- o .: "node"
        fields <- node .: "fields"
        fieldNodes <- fields .: "nodes"
        let statusField = findStatusField fieldNodes
        items <- node .: "items"
        itemNodes <- items .: "nodes"
        parsedItems <- mapM parseItem (itemNodes :: [Value])
        pure (parsedItems, statusField)

    findStatusField :: [Value] -> StatusField
    findStatusField nodes =
        case concatMap extractSF nodes of
            (sf : _) -> sf
            [] ->
                StatusField
                    { sfFieldId = ""
                    , sfOptions = []
                    }

    extractSF :: Value -> [StatusField]
    extractSF v =
        case parseMaybe sfParser v of
            Just sf -> [sf]
            Nothing -> []

    sfParser :: Value -> Parser StatusField
    sfParser = withObject "field" $ \o -> do
        name <- o .: "name"
        if (name :: Text) == "Status"
            then do
                fid <- o .: "id"
                opts <- o .: "options"
                pure
                    StatusField
                        { sfFieldId = fid
                        , sfOptions = opts
                        }
            else fail "not Status"

    parseItem :: Value -> Parser ProjectItem
    parseItem = withObject "item" $ \o -> do
        itemId <- o .: "id"
        fvs <- o .: "fieldValues"
        fvNodes <- fvs .: "nodes"
        let itemStatus = extractStatus fvNodes
        content <- o .: "content"
        parseContent itemId itemStatus content

    extractStatus :: [Value] -> Maybe KanbanStatus
    extractStatus = foldr go Nothing
      where
        go v acc = parseMaybe sParser v <|> acc
        sParser :: Value -> Parser KanbanStatus
        sParser = withObject "fv" $ \o -> do
            field <- o .: "field"
            fn <- field .: "name"
            if (fn :: Text) == "Status"
                then do
                    n <- o .: "name"
                    case textToStatus n of
                        Just s -> pure s
                        Nothing -> fail "unknown"
                else fail "not Status"

    parseContent
        :: Text
        -> Maybe KanbanStatus
        -> Value
        -> Parser ProjectItem
    parseContent itemId itemStatus =
        withObject "content" $ \o -> do
            itemTitle <- o .: "title"
            itemNumber <- o .:? "number"
            itemBody <- o .:? "body"
            mRepo <- o .:? "repository"
            mLabelsObj <- o .:? "labels"
            let itemType = case (itemNumber, mRepo) of
                    (Just _, Just _) -> IssueItem
                    _ -> DraftIssueItem
            itemLabels <- case mLabelsObj of
                Just lo -> do
                    ns <- lo .: "nodes"
                    mapM
                        (withObject "l" (.: "name"))
                        (ns :: [Value])
                Nothing -> pure []
            (itemRepoName, itemRepoOwner) <-
                case mRepo of
                    Just ro -> do
                        rn <- ro .: "name"
                        owner <- ro .: "owner"
                        rl <- owner .: "login"
                        pure (Just rn, Just rl)
                    Nothing -> pure (Nothing, Nothing)
            let itemDraftId =
                    if itemType == DraftIssueItem
                        then Just itemId
                        else Nothing
            pure ProjectItem{..}

parseDraftId :: Value -> Either Text Text
parseDraftId val =
    case parseMaybe parser val of
        Just i -> Right i
        Nothing -> Left "Failed to parse draft item ID"
  where
    parser :: Value -> Parser Text
    parser = withObject "data" $ \o -> do
        add <- o .: "addProjectV2DraftIssue"
        item <- add .: "projectV2Item"
        item .: "id"
