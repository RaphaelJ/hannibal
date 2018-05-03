-- Hannibal, a P2P client for local area networks.
--
-- Copyright (C) 2017 Raphael Javaux <raphaeljavaux@gmail.com>
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

module Hannibal.Network.Discovery
    ( getDiscoverySocket
    ) where

import ClassyPrelude

import Data.Bson (Val (..), Value (String))
import Network.Socket (
      AddrInfoFlag (AI_CANONNAME, AI_NUMERICSERV), Socket, SocketType (Datagram)
    , AddrInfo (addrFlags, addrProtocol, addrSocketType)
    , Family (AF_INET), defaultHints, getAddrInfo, socket
    )
import Text.Printf (printf)

import Hannibal.Config (Config (..))

-- | Announce the current client to the local network.
data AnnounceClient = AnnounceClient {
      clientName :: !Text
    } deriving (Eq, Show)

instance Val AnnounceClient where
    val = String . clientName

    cast' (String name) = Just $! AnnounceClient name
    cast' _             = Nothing

-- | Opens the discovery socket.
getDiscoverySocket :: MonadIO m => Config -> m Socket
getDiscoverySocket Config{..} = liftIO $! do
    let hints = defaultHints {
          addrFlags = [AI_CANONNAME, AI_NUMERICSERV]
        , addrSocketType = Datagram
        }

    addr:_ <- getAddrInfo (Just hints) (Just "localhost")
        (Just $! show cDiscoveryPort)

    printf "Opens UDP discovery socket on %s\n" (show addr)

    socket AF_INET Datagram (addrProtocol addr)

-- announceClient :: Socket -> ConduitT ByteString o WithInstanceIO ()
-- announceClient
