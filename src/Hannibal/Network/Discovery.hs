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
import Control.Monad.Logger (MonadLogger, logDebug)
import Data.Binary.Get (runGetOrFail)
import Data.UUID (UUID, fromByteString, toByteString)
import Network.Socket
    --   AddrInfoFlag (AI_CANONNAME, AI_NUMERICSERV)
    -- , AddrInfo (addrFlags, addrProtocol, addrSocketType),
    ( Family (AF_INET, AF_INET6)
    -- , Socket
    , SockAddr (SockAddrInet, SockAddrInet6)
    -- , SocketType (Datagram)
    -- , defaultProtocol, socket
    -- , defaultHints, getAddrInfo, setSocketOption
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

-- | The maximum size of an UDP discovery packet.
maxMessageSize :: Int
maxMessageSize = 65507 -- UDP on IPv4 maximum payload.

-- | Announce the current client instance to the local network.
data AnnounceInstance = AnnounceInstance {
      aiName    :: !Text
    , aiID      :: !UUID
    } deriving (Eq, Show)

newtype BsonUUID = BsonUUID { buVal :: UUID }
    deriving (Eq, Show)

instance B.Val BsonUUID where
    val = B.val . B.UUID . LBS.toStrict . toByteString . buVal

    cast' (B.Uuid (B.UUID bs)) =
        map BsonUUID $! fromByteString $! LBS.fromStrict bs
    cast' _ = Nothing

instance IsMessage AnnounceInstance where
    toMessage AnnounceInstance{..} = Message
        [ "name"    B.=: aiName
        , "id"      B.=: (B.UUID $! LBS.toStrict $! toByteString aiID)
        ]

    fromMessage (Message doc) =
        AnnounceInstance
            <$> doc B.!? "name"
            <*> (do
                B.UUID uuid <- doc B.!? "id"
                fromByteString $! LBS.fromStrict uuid)

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
    name <- cName <$> askConfig
    uuid <- iInstanceID <$> ask

    sock <- iDiscoverySocket <$> ask
    addr <- broadcastAddr AF_INET

    let msg = AnnounceInstance name uuid

    C.runConduit $!
             C.yield msg
        C..| conduitPutMessage
        C..| C.map (\bs ->
            assert (length bs <= maxMessageSize) (C.Message bs addr))
        C..| C.sinkToSocket sock

    let logMsg = pack $! printf
            "Broadcasted discovery announce to %v\n" (show addr)
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
    fork $! C.runConduit conduit
  where
    datagramSink :: (MonadIO m, MonadThrow m, MonadLogger m) =>
        C.ConduitT C.Message C.Void m ()
    datagramSink = C.mapM_ (\dgram ->
        readDatagram dgram >>= uncurry handleAnnounce)

    -- | Tries to read the UDP discovery datagram, or throws a
    -- `AnnounceDaemonException`.
    readDatagram :: MonadThrow m => C.Message -> m (SockAddr, AnnounceInstance)
    readDatagram (C.Message msgData msgAddr) =
        case runGetOrFail getMessage $! LBS.fromStrict msgData of
            Left (_, _, errorMsg) ->
                throw $! AnnounceDaemonException errorMsg
            Right (remaining, _, msg)
                | LBS.null remaining -> return (msgAddr, msg)
                | otherwise -> throw $!
                    AnnounceDaemonException "Packet continues after message."

    handleAnnounce :: (MonadLogger m, MonadIO m) =>
        SockAddr -> AnnounceInstance -> m ()
    handleAnnounce addr AnnounceInstance{..} = do
        let logMsg = pack $! printf
                "Received discovery announce from `%v` (id: %v, address: %v)\n"
                aiName (show aiID) (show addr)
        $(logDebug) logMsg

        --
        -- currentID <- iInstanceID <$> ask
        -- let !isCurrentClient = aiID == currentID
        --
        -- unless isCurrentClient $ do
        --     return ()
