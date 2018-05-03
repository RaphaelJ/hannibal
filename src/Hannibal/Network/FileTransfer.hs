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

module Hannibal.Network.FileTransfer
    ( putPiece
    ) where

import ClassyPrelude

-- import Codec.Compression.LZ4 (compress)
import Data.Binary (Put)
import Data.Bson (Binary (..), (=:))
import Data.Bson.Binary (putDocument)

import qualified Data.ByteString.Lazy as LBS

import Hannibal.Filesystem.Pieces (PieceDesc (..))

putPiece :: PieceDesc -> LByteString -> Put
putPiece !PieceDesc{..} content = do
    let Just content' = Just $! LBS.toStrict content

    putDocument [ "digest"    =: Binary pdDigest
                , "content"   =: Binary content'
                ]
