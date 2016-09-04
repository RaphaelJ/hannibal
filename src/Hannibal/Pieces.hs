-- Hannibal, a P2P client for local networks.
--
-- Copyright (C) 2016 Raphael Javaux <raphaeljavaux@gmail.com>
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

module Hannibal.Pieces where

import ClassyPrelude

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V

import Data.Digest.Pure.SHA (bytestringDigest, sha512)

pieceSize, chunkSize :: Integer
pieceSize = 4 * 1024 * 1024
chunkSize = 256 * 1024

digest :: BL.ByteString -> B.ByteString
digest = BL.toStrict . bytestringDigest . sha512

-- | File descriptor.
--
-- A file is decomposed in several pieces, and the digest of a file is the
-- digest of the concatenation of the digests of its pieces.
data FileDesc = FileDesc {
      fdDigest  :: !B.ByteString
    , fdSize    :: !Integer
    , fdPieces  :: !(V.Vector PieceDesc)
    } deriving (Eq, Show)

data PieceDesc = PieceDesc {
      pdDigest  :: !B.ByteString
    , pdSize    :: !Integer
    } deriving (Eq, Show)

-- | Given the content of a file, computes the file descriptor.
fileDesc :: BL.ByteString -> FileDesc
fileDesc content =
    let pieces     = map pieceDesc $ splitFile content
        fileDigest = digest $ BL.fromChunks $ map pdDigest pieces
        fileSize   = sum $ map pdSize pieces
    in FileDesc fileDigest fileSize (V.fromList pieces)

-- | Given the content of a file piece, computes the piece descriptor.
pieceDesc :: BL.ByteString -> PieceDesc
pieceDesc content = PieceDesc (digest content) (toInteger $ BL.length content)

-- | Splits the content of the file in several pieces.
splitFile :: BL.ByteString -> [BL.ByteString]
splitFile content
    | content == BL.empty = []
    | otherwise           =
        let (current, next) = BL.splitAt (fromInteger pieceSize) content
        in current : splitFile next
