{- |
Module      : Kanbanned
Description : Terminal kanban board for agent-daemon
-}
module Kanbanned
    ( runApp
    , CliOverrides (..)
    , noOverrides
    ) where

import Kanbanned.App (CliOverrides (..), noOverrides, runApp)
