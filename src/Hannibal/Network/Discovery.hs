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

module Hannibal.Network.Discovery
    ( announceInstance, discoveryDaemon
    ) where

import ClassyPrelude

import qualified Data.Bson as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Network.UDP as C

import Control.Exception.Safe (Exception)
import Control.Monad.Logger (MonadLogger, logDebug, logInfo)
import Data.Binary.Get (runGetOrFail)
import Network.Socket
    ( Family (AF_INET, AF_INET6)
    , SockAddr (SockAddrInet, SockAddrInet6)
    , iNADDR_ANY, iN6ADDR_ANY
    , bind, tupleToHostAddress, tupleToHostAddress6
    )
import Text.Printf (printf)

import Hannibal.Config (Config (..))
import Hannibal.Instance
    ( Instance (..), InstanceT, InstanceIO
    , askConfig
    )
import Hannibal.Network.Message
    ( Message (..), IsMessage (..), conduitPutMessage, getMessage
    )
import Hannibal.UUID (UUID)

-- | The maximum size of an UDP discovery packet.
maxMessageSize :: Int
maxMessageSize = 65507 -- UDP on IPv4 maximum payload.

-- | Announce the current client instance to the local network.
data AnnounceMessage = AnnounceMessage { amID :: !UUID }
    deriving (Eq, Show)

instance IsMessage AnnounceMessage where
    toMessage AnnounceMessage{..} = Message
        [ "type"    B.=: asText "announce"
        , "id"      B.=: amID
        ]

    fromMessage (Message doc) = AnnounceMessage <$> doc B.!? "id"

-- | Returns a broadcast socket address for the given socket protocol family.
broadcastAddr :: Monad m => Family -> InstanceT m SockAddr
broadcastAddr family = do
    discoveryPort <- cDiscoveryPort <$> askConfig
    return $! case family of
        AF_INET ->
            let ipv4Addr = (255, 255, 255, 255)
            in SockAddrInet discoveryPort (tupleToHostAddress ipv4Addr)
        AF_INET6 ->
             -- Link local broadcast (LAN)
            let ipv6Addr = (0xFF02, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0)
            in SockAddrInet6 discoveryPort 0 (tupleToHostAddress6 ipv6Addr) 0
        _ -> error "Socket family does not support broadcast"

-- | Returns a socket address that can be used to listen to the discovery UDP
-- messages.
listenAddr :: Monad m => Family -> InstanceT m SockAddr
listenAddr family = do
    discoveryPort <- cDiscoveryPort <$> askConfig
    return $! case family of
        AF_INET -> SockAddrInet discoveryPort iNADDR_ANY
        AF_INET6 -> SockAddrInet6 discoveryPort 0 iN6ADDR_ANY 0
        _ -> error "Socket family not supported"

-- | Sends an UDP broadcast message announcing the client to the local network.
announceInstance :: InstanceIO ()
announceInstance = do
    uuid <- iInstanceID <$> ask

    sock <- iDiscoverySocket <$> ask
    addr <- broadcastAddr AF_INET

    let msg = AnnounceMessage uuid

    C.runConduit $!
             C.yield msg
        C..| conduitPutMessage
        C..| C.map (\bs ->
            assert (length bs <= maxMessageSize) (C.Message bs addr))
        C..| C.sinkToSocket sock

    let logMsg = pack $! printf
            "Broadcasted discovery announce to %v" (show addr)
    $(logDebug) logMsg

newtype AnnounceDaemonException = AnnounceDaemonException String
    deriving Show

instance Exception AnnounceDaemonException

-- | Forks a thread that repetitively listens to UDP messages that announce
-- Hannibal clients.
discoveryDaemon :: InstanceIO ThreadId
discoveryDaemon = do
    sock <- iDiscoverySocket <$> ask
    addr <- listenAddr AF_INET

    liftIO $! bind sock addr

    let conduit = C.sourceSocket sock maxMessageSize C..| datagramSink
    threadId <- fork $! C.runConduit conduit

    let logMsg = pack $! printf
            "Discovery deamon listening on `%v`" (show addr)
    $(logInfo) logMsg

    return threadId
  where
    datagramSink :: (MonadIO m, MonadThrow m, MonadLogger m) =>
        C.ConduitT C.Message C.Void m ()
    datagramSink = C.mapM_ (\dgram ->
        readDatagram dgram >>= uncurry handleAnnounce)

    -- | Tries to read the UDP discovery datagram, or throws a
    -- `AnnounceDaemonException`.
    readDatagram :: MonadThrow m => C.Message -> m (SockAddr, AnnounceMessage)
    readDatagram (C.Message msgData msgAddr) =
        case runGetOrFail getMessage $! LBS.fromStrict msgData of
            Left (_, _, errorMsg) ->
                throw $! AnnounceDaemonException errorMsg
            Right (remaining, _, msg)
                | LBS.null remaining -> return (msgAddr, msg)
                | otherwise -> throw $!
                    AnnounceDaemonException "Packet continues after message."

    handleAnnounce :: (MonadLogger m, MonadIO m) =>
        SockAddr -> AnnounceMessage -> m ()
    handleAnnounce addr AnnounceMessage{..} = do
        $(logDebug) $! pack $! printf
            "Received discovery announce (id: %v, address: %v)"
            (show amID) (show addr)

        --
        -- currentID <- iInstanceID <$> ask
        -- let !isCurrentClient = amID == currentID
        --
        -- unless isCurrentClient $ do
        --     return ()
