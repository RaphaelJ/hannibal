-- Hannibal, a P2P client for local area networks.
--
-- Copyright (C) 2016, 2017 Raphael Javaux <raphaeljavaux@gmail.com>
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

module Hannibal.Filesystem.Pieces
    ( FileDesc (..), PieceDesc (..)
    , pieceSize
    , fileDesc, pieceDesc, splitFile
    ) where

import ClassyPrelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V

import Data.Digest.Pure.SHA (bytestringDigest, sha512)

newtype Digest = Digest ByteString
    deriving (Eq, Ord, Read, Show)

pieceSize :: Integer
pieceSize = 4 * 1024 * 1024     -- 4 MB, in bytes

digest :: LByteString -> ByteString
digest = Digest . LBS.toStrict . bytestringDigest . sha512

-- | File descriptor.
--
-- A file is decomposed in several pieces, and the digest of a file is the
-- digest of the concatenation of the digests of its pieces.
data FileDesc = FileDesc
    { fdDigest  :: !Digest
    , fdSize    :: !Integer
    , fdPieces  :: !(V.Vector PieceDesc)
    } deriving (Eq, Show)

data PieceDesc = PieceDesc
    { pdDigest  :: !Digest
    , pdSize    :: !Integer
    } deriving (Eq, Show)

-- | Given the content of a file, computes the file descriptor.
fileDesc :: LByteString -> FileDesc
fileDesc content =
    let pieces     = map pieceDesc $ splitFile content
        fileDigest = digest $ LBS.fromChunks $ map pdDigest pieces
        fileSize   = sum $ map pdSize pieces
    in FileDesc fileDigest fileSize (V.fromList pieces)

-- | Given the content of a file piece, computes the piece descriptor.
pieceDesc :: LByteString -> PieceDesc
pieceDesc content =
    let len = toInteger $ LBS.length content
    in assert (len <= pieceSize) $! PieceDesc (digest content) len

-- | Splits the content of a file in several pieces.
splitFile :: LByteString -> [LByteString]
splitFile content
    | LBS.null content   = []
    | otherwise         =
        let (current, next) = LBS.splitAt (fromInteger pieceSize) content
        in current : splitFile next
