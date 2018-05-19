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

module Hannibal.UUID
    ( UUID (..)
    ) where

import ClassyPrelude

import qualified Data.Bson as B
import qualified Data.ByteString.Lazy as LBS
import qualified Data.UUID as UUID

import System.Random (Random (..))

-- | A wrappers over `UUID` that supports the BSON serialization.
newtype UUID = UUID { uuidVal :: UUID.UUID }
    deriving (Eq, Ord, Show, Typeable)

instance B.Val UUID where
    val = B.Uuid . B.UUID . LBS.toStrict . UUID.toByteString . uuidVal

    cast' (B.Uuid (B.UUID bs)) =
        map UUID $! UUID.fromByteString $! LBS.fromStrict bs
    cast' _ = Nothing

instance Random UUID where
    randomR (UUID mi, UUID ma) = first UUID . randomR (mi, ma)
    random = first UUID . random
