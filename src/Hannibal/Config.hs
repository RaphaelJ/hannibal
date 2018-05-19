-- Hannibal, a P2P client for local area networks.
--
-- Copyright (C) 2016, 2017, 2018 Raphael Javaux <raphaeljavaux@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>

module Hannibal.Config
    ( Config (..), Logger (..)
    , defaultConfig
    ) where

import ClassyPrelude

import qualified Data.Map.Strict as M

import Network.Socket (PortNumber)

data Logger = StderrLogger | StdoutLogger
    deriving (Eq, Read, Show)

data Config = Config
    {
    -- | The name of this client
      cName             :: !Text
    -- | The UDP port used to discover local clients.
    , cDiscoveryPort    :: !PortNumber
    -- | The TCP port used to communicate with other clients.
    --
    -- If `Nothing`, uses a random availaible TCP port.
    , cControlPort      :: !(Maybe PortNumber)
    -- | The directories that are shared by this instance.
    --
    -- Maps shared directory names to directory paths.
    , cSharedDirs       :: M.Map Text FilePath
    -- The logger transformers used by the application.
    , cLogger           :: Logger
    } deriving (Eq, Read, Show)

defaultConfig :: Config
defaultConfig = Config "Unamed client" 60391 Nothing M.empty StdoutLogger
