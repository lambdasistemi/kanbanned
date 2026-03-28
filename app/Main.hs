{- |
Module      : Main
Description : Entry point for kanbanned TUI
-}
module Main (main) where

import Data.Text qualified as T
import Kanbanned (CliOverrides (..), runApp)
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

optionsParser :: Parser CliOverrides
optionsParser =
    CliOverrides
        <$> optional
            ( T.pack
                <$> strOption (long "token" <> short 't')
            )
        <*> optional
            ( T.pack
                <$> strOption (long "server" <> short 's')
            )
        <*> optional
            ( T.pack
                <$> strOption
                    (long "project" <> short 'p')
            )

main :: IO ()
main = do
    opts <-
        execParser $
            info
                (optionsParser <**> helper)
                ( fullDesc
                    <> progDesc "Terminal kanban board"
                    <> header "kanbanned"
                )
    runApp opts
