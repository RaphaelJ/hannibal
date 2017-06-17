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
-- but WITHOUT ANY WA2RRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>

module Hannibal.Filesystem.Filesystem
    ( Cursor
    , listDirectory
    ) where

import ClassyPrelude

import System.FilePath.Posix (FilePath)

import Hannibal.Index (SharedDirectory)

data Cursor = FileCursor !SharedDirectory !FilePath
            | DirCursor  !SharedDirectory !FilePath
    deriving (Eq, Read, Show)

listDirectory :: Cursor -> IO [Cursor]
listDirectory cursor = do
