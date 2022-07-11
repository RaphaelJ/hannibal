-- Hannibal, a P2P client for local area networks.
--
-- Copyright (C) 2017, 2018 Raphael Javaux <raphaeljavaux@gmail.com>
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

module Hannibal.Network.Discovery (
    announceInstance,
    discoveryServer,
) where

import ClassyPrelude

import qualified Data.Bson as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Network.UDP as C

import Conduit (MonadThrow, throwM)
import Control.Concurrent (ThreadId)
import Control.Monad.Logger (MonadLogger, logDebug, logInfo)
import Data.Binary.Get (runGetOrFail)
import Network.Socket (
    Family (AF_INET, AF_INET6),
    SockAddr (SockAddrInet, SockAddrInet6),
    bind,
    tupleToHostAddress,
    tupleToHostAddress6,
 )
import Text.Printf (printf)

import Hannibal.Config (Config (..))
import Hannibal.Instance (Instance (..), InstanceIO, InstanceT, askConfig, forkInstance)
import Hannibal.Network.Message (
    IsMessage (..),
    SerializedMessage (..),
    conduitPutMessage,
    getMessage,
 )
import Hannibal.UUID (UUID)

-- | The maximum size of an UDP discovery packet.
maxMessageSize :: Int
maxMessageSize = 65507 -- UDP on IPv4 maximum payload.

-- | Announce the current client instance to the local network.
data AnnounceMessage = AnnounceMessage {amID :: !UUID, amName :: Text}
    deriving (Eq, Show)

instance IsMessage AnnounceMessage where
    serializeMessage AnnounceMessage{..} =
        SerializedMessage
            [ "type" B.=: asText "announce"
            , "id" B.=: amID
            , "name" B.=: amName
            ]

    deserializeMessage (SerializedMessage doc) =
        AnnounceMessage <$> doc B.!? "id" <*> doc B.!? "name"

-- | Returns a broadcast socket address for the given socket protocol family.
broadcastAddr :: Monad m => Family -> InstanceT m SockAddr
broadcastAddr family = do
    discoveryPort <- cDiscoveryPort <$> askConfig
    return $! case family of
        AF_INET ->
            let ipv4Addr = tupleToHostAddress (255, 255, 255, 255)
             in SockAddrInet discoveryPort ipv4Addr
        AF_INET6 ->
            -- Link local broadcast (LAN)
            let ipv6Addr = tupleToHostAddress6 (0xFF02, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
             in SockAddrInet6 discoveryPort 0 ipv6Addr 0
        _ -> error "Socket family does not support broadcast"

{- | Returns a socket address that can be used to listen to the discovery UDP
 messages.
-}
listenAddr :: Monad m => Family -> InstanceT m SockAddr
listenAddr family = do
    discoveryPort <- cDiscoveryPort <$> askConfig
    return $! case family of
        AF_INET ->
            let ipv4Addr = tupleToHostAddress (0, 0, 0, 0)
             in SockAddrInet discoveryPort ipv4Addr
        AF_INET6 ->
            let ipv6Addr = tupleToHostAddress6 (0, 0, 0, 0, 0, 0, 0, 0)
             in SockAddrInet6 discoveryPort 0 ipv6Addr 0
        _ -> error "Socket family not supported"

-- | Sends an UDP broadcast message announcing the client to the local network.
announceInstance :: InstanceIO ()
announceInstance = do
    uuid <- iInstanceID <$> ask
    name <- cName . iConfig <$> ask

    sock <- iDiscoverySocket <$> ask
    addr <- broadcastAddr AF_INET

    let msg = AnnounceMessage uuid name

    C.runConduit
        $! C.yield msg
        C..| conduitPutMessage
        C..| C.map
            ( \bs ->
                assert (length bs <= maxMessageSize) (C.Message bs addr)
            )
        C..| C.sinkToSocket sock

    $(logDebug) $! pack
        $! printf
            "Broadcasted discovery announce to %v"
            (show addr)

newtype AnnounceDaemonException = AnnounceDaemonException String
    deriving (Show)

instance Exception AnnounceDaemonException

{- | Forks a thread that repetitively listens to UDP messages that announce
 Hannibal clients.
-}
discoveryServer :: InstanceIO ThreadId
discoveryServer = do
    sock <- iDiscoverySocket <$> ask
    addr <- listenAddr AF_INET

    liftIO $! bind sock addr

    let conduit = C.sourceSocket sock maxMessageSize C..| datagramSink
    threadId <- forkInstance $! C.runConduit conduit

    $(logInfo) $! pack
        $! printf
            "Discovery deamon listening on `%v`"
            (show addr)

    return threadId
  where
    datagramSink ::
        (MonadIO m, MonadThrow m, MonadLogger m) =>
        C.ConduitT C.Message C.Void m ()
    datagramSink = C.mapM_ $ readDatagram >=> uncurry handleAnnounce

    readDatagram :: MonadThrow m => C.Message -> m (SockAddr, AnnounceMessage)
    readDatagram (C.Message msgData msgAddr) =
        case runGetOrFail getMessage $! LBS.fromStrict msgData of
            Left (_, _, errorMsg) ->
                throwM $! AnnounceDaemonException errorMsg
            Right (remaining, _, msg)
                | LBS.null remaining -> return (msgAddr, msg)
                | otherwise ->
                    throwM $! AnnounceDaemonException "Packet continues after message."

    handleAnnounce ::
        (MonadLogger m, MonadIO m) =>
        SockAddr ->
        AnnounceMessage ->
        m ()
    handleAnnounce addr AnnounceMessage{..} = do
        $(logDebug) $! pack
            $! printf
                "Received discovery announce (id: %v, address: %v)"
                (show amID)
                (show addr)
