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

module Hannibal.Instance
    ( Instance (..), InstanceT, InstanceIO
    , getInstance, runWithInstance, askConfig
    ) where

import ClassyPrelude

import Control.Monad.Reader (ReaderT, runReaderT)
import Data.UUID (UUID)
import Network.Socket
      -- AddrInfoFlag (AI_CANONNAME, AI_NUMERICSERV)
    -- , AddrInfo (addrFlags, addrProtocol, addrSocketType)
    ( Family (AF_INET)
    , Socket
    -- , SockAddr (SockAddrInet, SockAddrInet6)
    , SocketOption (Broadcast)
    , SocketType (Datagram)
    , defaultProtocol, setSocketOption, socket
    -- , defaultHints, getAddrInfo, setSocketOption
    -- , tupleToHostAddress, tupleToHostAddress6
    )
import System.Random (randomIO)
import Text.Printf (printf)

import Hannibal.Config (Config (..))

-- | The main runtime structure for the Hannibal client.
data Instance = Instance
    { iConfig           :: !Config
    -- | Nonpersistent unique identifer of the instance generated on startup.
    , iInstanceID       :: !UUID
    -- | The UDP socket used to discover other local clients.
    , iDiscoverySocket  :: !Socket
    } deriving (Eq, Show)

getInstance :: MonadIO m => Config -> m Instance
getInstance config =
    Instance config <$> liftIO randomIO
                    <*> getDiscoverySocket config

-- | Opens the discovery socket.
getDiscoverySocket :: MonadIO m => Config -> m Socket
getDiscoverySocket Config{..} = liftIO $! do
    -- let hints = defaultHints {
    --       addrFlags = [AI_CANONNAME, AI_NUMERICSERV]
    --     , addrSocketType = Datagram
    --     }
    --
    -- addr:_ <- getAddrInfo (Just hints) (Just "localhost")
    --     (Just $! show cDiscoveryPort)

    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock Broadcast 1

    printf "Opens UDP discovery socket on %s\n" (show sock)

    return sock

-- | Wrapper over `ReaderT` that provides the
type InstanceT = ReaderT Instance
type InstanceIO = InstanceT IO

runWithInstance :: InstanceT m a -> Instance -> m a
runWithInstance = runReaderT

-- | Helper that use `Reader`'s `ask` to get the instance's `Config`.
askConfig :: Monad m => InstanceT m Config
askConfig = iConfig <$> ask
