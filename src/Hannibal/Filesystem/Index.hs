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

module Hannibal.Filesystem.Index
    ( Index (..), IndexedDirectory (..)
    , newIndex, addDirectory
    ) where

import ClassyPrelude hiding (Index)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M

import System.Directory.Tree (AnchoredDirTree (..), readDirectoryWith)

import Hannibal.Filesystem.Pieces (FileDesc, fileDesc)

data Index = Index
    {
    -- | The name of the current node, as seen by the other nodes.
      iNodeName     :: !Text
    -- | The list of the directories that are shared by the current node.
    --
    -- Maps the names of the shared directories to a in-memory tree that
    -- contains the digest of the contained files
    , iSharedDirs   :: !(M.Map Text IndexedDirectory)
    } deriving (Eq, Show)

newtype IndexedDirectory = IndexedDirectory (AnchoredDirTree FileDesc)
    deriving (Eq, Show)

--

-- | Creates an new empty index.
newIndex :: Text -> Index
newIndex nodeName = Index nodeName M.empty

-- | Adds and indexes a new shared directory to the index.
addDirectory :: Text -> FilePath -> Index -> IO Index
addDirectory dirName path idx@Index{..} = do
    cache <- IndexedDirectory <$> readDirectoryWith indexFile path
    return $! idx { iSharedDirs = M.insert dirName cache iSharedDirs }
  where
    indexFile filePath = fileDesc <$> LBS.readFile filePath
