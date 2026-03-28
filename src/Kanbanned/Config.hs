{- |
Module      : Kanbanned.Config
Description : Configuration file management
-}
module Kanbanned.Config
    ( Config (..)
    , defaultConfig
    , loadConfig
    , saveConfig
    , configPath
    ) where

import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , eitherDecodeFileStrict'
    , encodeFile
    , object
    , withObject
    , (.:?)
    , (.=)
    )
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory
    ( createDirectoryIfMissing
    , doesFileExist
    , getXdgDirectory
    )
import System.Directory qualified as Dir
import System.FilePath ((</>))

-- | Application configuration
data Config = Config
    { cfgGitHubToken :: !(Maybe Text)
    , cfgAgentServer :: !Text
    , cfgProjectId :: !(Maybe Text)
    , cfgRefreshInterval :: !Int
    }
    deriving stock (Show, Eq)

instance FromJSON Config where
    parseJSON = withObject "Config" $ \o ->
        Config
            <$> o .:? "github_token"
            <*> (fromMaybe (cfgAgentServer defaultConfig) <$> o .:? "agent_server")
            <*> o .:? "project_id"
            <*> ( fromMaybe (cfgRefreshInterval defaultConfig)
                    <$> o .:? "refresh_interval"
                )

instance ToJSON Config where
    toJSON Config{..} =
        object
            [ "github_token" .= cfgGitHubToken
            , "agent_server" .= cfgAgentServer
            , "project_id" .= cfgProjectId
            , "refresh_interval" .= cfgRefreshInterval
            ]

-- | Default configuration
defaultConfig :: Config
defaultConfig =
    Config
        { cfgGitHubToken = Nothing
        , cfgAgentServer = "http://localhost:8080"
        , cfgProjectId = Nothing
        , cfgRefreshInterval = 30
        }

-- | Get config file path
configPath :: IO FilePath
configPath = do
    dir <- getXdgDirectory Dir.XdgConfig "kanbanned"
    pure $ dir </> "config.json"

-- | Load configuration from file
loadConfig :: IO Config
loadConfig = do
    path <- configPath
    exists <- doesFileExist path
    if exists
        then do
            result <- eitherDecodeFileStrict' path
            case result of
                Right cfg -> pure cfg
                Left _ -> pure defaultConfig
        else pure defaultConfig

-- | Save configuration to file
saveConfig :: Config -> IO ()
saveConfig cfg = do
    path <- configPath
    dir <- getXdgDirectory Dir.XdgConfig "kanbanned"
    createDirectoryIfMissing True dir
    encodeFile path cfg
