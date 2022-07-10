-- Hannibal, a P2P client for local area networks.
--
-- Copyright (C) 2022 Raphael Javaux <raphaeljavaux@gmail.com>
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
module Hannibal.Network.Sockets (
    getControlSocket,
    getDiscoverySocket,
) where

import ClassyPrelude
import Network.Socket (
    Family (AF_INET),
    Socket,
    SocketOption (
        Broadcast,
        ReusePort
    ),
    SocketType (Datagram, Stream),
    defaultProtocol,
    setSocketOption,
    socket,
 )

-- | Opens the control TCP socket.
getControlSocket :: MonadIO m => m Socket
getControlSocket = liftIO $! socket AF_INET Stream defaultProtocol

-- | Opens the discovery UDP socket.
getDiscoverySocket :: MonadIO m => m Socket
getDiscoverySocket =
    liftIO $! do
        sock <- socket AF_INET Datagram defaultProtocol
        setSocketOption sock Broadcast 1
        setSocketOption sock ReusePort 1
        return sock
