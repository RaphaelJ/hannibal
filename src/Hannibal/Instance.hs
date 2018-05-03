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

module Hannibal.Instance where

import ClassyPrelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Network.Socket (Socket)

import Hannibal.Config (Config (..))
import Hannibal.Network.Discovery (getDiscoverySocket)

-- | The main runtime structure for Hannibal.
data Instance = Instance
    { iConfig           :: !Config
    -- | The UDP socket used to discover other local clients.
    , iDiscoverySocket  :: Socket
    } deriving (Eq, Show)

type RuntimeIO = ReaderT Instance IO

runWithInstance :: RuntimeIO a -> Instance -> IO a
runWithInstance = runReaderT

getInstance :: MonadIO m => Config -> m Instance
getInstance config = Instance config <$> getDiscoverySocket config
