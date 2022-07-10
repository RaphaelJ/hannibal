-- Hannibal, a P2P client for local area networks.
--
-- Copyright (C) 2016, 2017, 2018, 2022 Raphael Javaux <raphaeljavaux@gmail.com>
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

module Hannibal.Instance (
    Instance (..),
    InstanceT,
    InstanceIO,
    getInstance,
    runWithInstance,
    askConfig,
    forkInstance,
) where

import ClassyPrelude

import Control.Concurrent (ThreadId, forkIO)
import Control.Monad.Logger (
    LoggingT (..),
    logInfo,
    runStderrLoggingT,
    runStdoutLoggingT,
 )
import Network.Socket (
    Socket,
 )
import System.Random (randomIO)
import Text.Printf (printf)

import Hannibal.Config (Config (..), Logger (..))
import Hannibal.Network.Sockets (getControlSocket, getDiscoverySocket)
import Hannibal.UUID (UUID)

-- | The main runtime structure for the Hannibal client.
data Instance = Instance
    { iConfig :: !Config
    , -- | Nonpersistent unique identifer of the instance generated on startup.
      iInstanceID :: !UUID
    , -- | The UDP socket used to discover other local clients.
      iDiscoverySocket :: !Socket
    , -- | The TCP socket used to communicate with other clients.
      iControlServerSocket :: !Socket
    }
    deriving (Eq, Show)

-- | Creates a new `Instance`.
getInstance :: MonadIO m => Config -> m Instance
getInstance config =
    Instance config <$> liftIO randomIO
        <*> getDiscoverySocket
        <*> getControlSocket

-- | Wrapper over `ReaderT` and `LoggingT` that provides reading and logging abilities.
type InstanceT m = LoggingT (ReaderT Instance m)

type InstanceIO = InstanceT IO

-- | Runs Hannibal monad with the given instance.
runWithInstance ::
    MonadIO m =>
    InstanceT m a ->
    Instance ->
    m a
runWithInstance action inst =
    let logger = cLogger $! iConfig inst
        loggerT = case logger of
            StdoutLogger -> runStdoutLoggingT
            StderrLogger -> runStderrLoggingT
        action' = do
            let uuid = iInstanceID inst
            $(logInfo) $! pack $! printf "Runs instance (id: `%v`)" (show uuid)

            action
     in runReaderT (loggerT action') inst

-- | Fork the current instance in a new lightweight thread.
forkInstance :: MonadIO m => InstanceIO () -> InstanceT m ThreadId
forkInstance action = do
    inst <- ask
    liftIO $! forkIO $! runWithInstance action inst

-- | Helper that use `Reader`'s `ask` to get the instance's `Config`.
askConfig :: Monad m => InstanceT m Config
askConfig = iConfig <$> ask
