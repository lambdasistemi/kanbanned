{- |
Module      : Main
Description : Entry point for kanbanned TUI
-}
module Main (main) where

import Kanbanned (runApp)
import Options.Applicative
    ( Parser
    , execParser
    , fullDesc
    , header
    , helper
    , info
    , long
    , optional
    , progDesc
    , short
    , strOption
    , (<**>)
    )

-- | CLI options
data Options = Options
    { _optToken :: Maybe String
    , _optServer :: Maybe String
    , _optProject :: Maybe String
    }

optionsParser :: Parser Options
optionsParser =
    Options
        <$> optional
            ( strOption
                ( long "token"
                    <> short 't'
                )
            )
        <*> optional
            ( strOption
                ( long "server"
                    <> short 's'
                )
            )
        <*> optional
            ( strOption
                ( long "project"
                    <> short 'p'
                )
            )

main :: IO ()
main = do
    _opts <-
        execParser $
            info
                (optionsParser <**> helper)
                ( fullDesc
                    <> progDesc "Terminal kanban board"
                    <> header "kanbanned"
                )
    runApp
