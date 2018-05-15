-- Hannibal, a P2P client for local area networks.
--
-- Copyright (C) 2018 Raphael Javaux <raphaeljavaux@gmail.com>
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

module Hannibal.Network.Control
    ( controlDaemon
    ) where

import ClassyPrelude

import Control.Monad.Logger (logInfo)
import Data.Conduit.Network (forkTCPServer)
import Data.Streaming.Network
    ( serverSettingsTCPSocket, appLocalAddr, appSockAddr
    )
import Network.Socket
    ( Family (AF_INET, AF_INET6), PortNumber
    , SockAddr (SockAddrInet, SockAddrInet6)
    , iNADDR_ANY, iN6ADDR_ANY, bind, getSocketName, listen, maxListenQueue
    )
import Text.Printf (printf)

import Hannibal.Config (Config (..))
import Hannibal.Instance (Instance (..), InstanceT, InstanceIO, askConfig)

-- | Returns a socket address that can be used to listen to control TCP
-- connections.
listenAddr :: Monad m => Family -> InstanceT m SockAddr
listenAddr family = do
    -- Uses port `0` when not defined to automatically allocate a port.
    controlPort <- (fromMaybe 0 . cControlPort) <$> askConfig
    return $! case family of
        AF_INET -> SockAddrInet controlPort iNADDR_ANY
        AF_INET6 -> SockAddrInet6 controlPort 0 iN6ADDR_ANY 0
        _ -> error "Socket family not supported"

listenQueueSize :: Int
listenQueueSize = max maxListenQueue 32

-- | Forks a thread that repetitively listens to incomming TCP connections on
-- the control socket.
--
-- Returns the port the thread ID of the server and the TCP port it's listening
-- on.
controlDaemon :: InstanceIO (ThreadId, PortNumber)
controlDaemon = do
    sock <- iControlSocket <$> ask
    addr <- listenAddr AF_INET

    liftIO $! do
        bind sock addr
        listen sock listenQueueSize

    -- When provided `0` as a port number, `bind` will randomly choose a port.
    let setts = serverSettingsTCPSocket sock

    threadId <- forkTCPServer setts handleClient

    (addr', port) <- getSocketAddrPort sock

    let logMsg = pack $! printf "Control deamon listening on `%v`," (show addr')
    $(logInfo) logMsg

    return (threadId, port)
  where
    handleClient appData = do
        print (appSockAddr appData)
        print (appLocalAddr appData)

    getSocketAddrPort sock = do
        addr <- liftIO $! getSocketName sock
        let port = case addr of
                SockAddrInet port' _ -> port'
                SockAddrInet6 port' _ _ _ -> port'
                _ -> error "Socket family not supported"
        return (addr, port)
