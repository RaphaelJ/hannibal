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
    ( announceInstance
    ) where

import ClassyPrelude

import qualified Data.Bson as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.Network.UDP as C

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
    , tupleToHostAddress, tupleToHostAddress6
    )
import Text.Printf (printf)

import Hannibal.Config (Config (..))
import Hannibal.Instance
    ( Instance (..), InstanceT, InstanceIO
    , askConfig
    )
import Hannibal.Network.Message
    ( Message (..), IsMessage (..), conduitPutMessage
    )

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

announceInstance :: InstanceIO ()
  -- -> ConduitT ByteString o WithInstanceIO ()
announceInstance = do
    name <- cName <$> askConfig
    uuid <- iInstanceID <$> ask

    sock <- iDiscoverySocket <$> ask
    addr <- broadcastAddr AF_INET

    let msg = AnnounceInstance name uuid

    liftIO $ printf "Broadcast message `%s` to %s\n"
        (show $ toMessage msg) (show addr)

    C.runConduit $!
             C.yield msg
        C..| conduitPutMessage
        C..| C.map (\bs -> C.Message bs addr)
        C..| C.sinkToSocket sock
