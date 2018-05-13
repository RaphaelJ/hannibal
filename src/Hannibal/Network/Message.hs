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

module Hannibal.Network.Message
    ( Message (..), IsMessage (..)
    , getMessage, putMessage
    , conduitGetMessage, conduitPutMessage
    ) where

import ClassyPrelude

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C

import Data.Binary (Binary (..), Get, Put)
import Data.Bson (Document)
import Data.Bson.Binary (getDocument, putDocument)
import Data.Conduit.Serialization.Binary (conduitGet, conduitPut)

-- | A network BSON message.
--
-- All network transmissions done by Hannibal are BSON documents.
newtype Message = Message { mDocument :: Document }
    deriving (Eq, Ord, Show)

instance Binary Message where
    put = putDocument . mDocument
    get = Message <$> getDocument

-- | Converts data-structures to and from `Message`s.
class IsMessage msg where
    toMessage :: msg -> Message
    fromMessage :: Message -> Maybe msg

instance IsMessage Message where
    toMessage = id
    fromMessage = Just

-- | Unserializes a `Message`.
getMessage :: IsMessage msg => Get msg
getMessage = do
    mMsg <- fromMessage <$> get
    case mMsg of
        Just msg -> return msg
        Nothing -> fail "Failed to read message."

-- | Serializes a `Message`.
putMessage :: IsMessage msg => msg -> Put
putMessage = put . toMessage

conduitPutMessage :: (IsMessage msg, Monad m) => C.ConduitT msg ByteString m ()
conduitPutMessage = C.map putMessage C..| conduitPut

conduitGetMessage :: (IsMessage msg, MonadThrow m) =>
    C.ConduitT ByteString msg m ()
conduitGetMessage = conduitGet getMessage
