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
module Hannibal.Network.Control (
    controlDaemon,
) where

import ClassyPrelude

import qualified Data.Bson as B
import qualified Data.Conduit as C

import Control.Monad.Logger (logInfo)
import Data.Conduit.Network (appSink, appSockAddr, appSource, forkTCPServer)
import Data.Streaming.Network (serverSettingsTCPSocket)
import Network.Socket (
    Family (AF_INET, AF_INET6),
    PortNumber,
    SockAddr (SockAddrInet, SockAddrInet6),
    bind,
    getSocketName,
    listen,
    maxListenQueue,
 )
import Text.Printf (printf)

import Hannibal.Config (Config (..))
import Hannibal.Instance (Instance (..), InstanceIO, InstanceT, askConfig)
import Hannibal.Network.Message (
    IsMessage (..),
    SerializedMessage (..),
    conduitGetMessage,
    conduitPutMessage,
 )
import Hannibal.UUID (UUID)

--
-- Messages
--

data ControlMessage = -- | Authenticates the Hannibal instance to the other peer.
    --
    -- This is sent by both remote parts when the connection is initiated.
    HelloMessage
    { -- | The name of the instance.
      hmName :: !Text
    , -- | The ID of the instance.
      hmID :: !UUID
    }
    deriving (Eq, Show)

instance IsMessage ControlMessage where
    toMessage HelloMessage{..} =
        Message
            [ "type" B.=: asText "hello"
            , "name" B.=: hmName
            , "id" B.=: hmID
            ]

    fromMessage (Message doc) = do
        type_ <- doc B.!? "type" :: Maybe Text

        case type_ of
            "hello" -> HelloMessage <$> doc B.!? "name" <*> doc B.!? "id"
            _ -> Nothing

--
-- Functions
--

{- | Returns a socket address that can be used to listen to control TCP
 connections.
-}
listenAddr :: Monad m => Family -> InstanceT m SockAddr
listenAddr family = do
    -- Uses port `0` when not defined to automatically allocate a port.
    controlPort <- (fromMaybe 0 . cControlPort) <$> askConfig
    return $! case family of
        AF_INET ->
            let ipv4Addr = tupleToHostAddress (0, 0, 0, 0)
             in SockAddrInet controlPort ipv4Addr
        AF_INET6 ->
            let ipv6Addr = tupleToHostAddress6 (0, 0, 0, 0, 0, 0, 0, 0)
             in SockAddrInet6 controlPort 0 ipv6Addr 0
        _ -> error "Socket family not supported"

listenQueueSize :: Int
listenQueueSize = max maxListenQueue 128

helloMessage :: Monad m => InstanceT m ControlMessage
helloMessage = do
    inst <- ask
    config <- askConfig
    return $! HelloMessage (cName config) (iInstanceID inst)

--
-- Handle client connection
--

-- | Handles an incomming client connection from a peer.
clientHandle ::
    (MonadIO m, MonadThrow m) =>
    SockAddr ->
    C.ConduitT ControlMessage ControlMessage (InstanceT m) ()
clientHandle peerAddr = do
    $(logInfo) $! pack
        $! printf
            "Handle incoming control link from `%v`"
            (show peerAddr)

    -- Waits for the hello message, and respond with another hello message.
    inHelloMsg <- C.await
    case inHelloMsg of
        Just (HelloMessage{..}) -> lift helloMessage >>= C.yield
        _ -> error "Expected hello message"

--
-- Daemon
--

{- | Forks a thread that repetitively listens to incomming TCP connections on
 the control socket.

 Returns the port the thread ID of the server and the TCP port it's listening
 on.
-}
controlDaemon :: InstanceIO (ThreadId, PortNumber)
controlDaemon = do
    sock <- iControlSocket <$> ask
    addr <- listenAddr AF_INET

    liftIO $! do
        bind sock addr
        listen sock listenQueueSize

    -- When provided `0` as a port number, `bind` will randomly choose a port.
    let setts = serverSettingsTCPSocket sock

    threadId <- forkTCPServer setts connectionHandle

    (addr', port) <- getSocketAddrPort sock

    let logMsg = pack $! printf "Control deamon listening on `%v`" (show addr')
    $(logInfo) logMsg

    return (threadId, port)
  where
    connectionHandle appData =
        C.runConduit
            $! appSource appData
            C..| conduitGetMessage
            C..| clientHandle (appSockAddr appData)
            C..| conduitPutMessage
            C..| appSink appData

    getSocketAddrPort sock = do
        addr <- liftIO $! getSocketName sock
        let port = case addr of
                SockAddrInet port' _ -> port'
                SockAddrInet6 port' _ _ _ -> port'
                _ -> error "Socket family not supported"
        return (addr, port)
