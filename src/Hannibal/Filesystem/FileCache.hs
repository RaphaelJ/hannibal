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

module Hannibal.Filesystem.Index where

import ClassyPrelude hiding (Index)

import qualified Data.Map as M

import Hannibal.Pieces (FileDesc)

data SharedDirectory = SharedDirectory {
      sdName    :: !Text
    , sdPath    :: FilePath
    , sdCache   ::
    }

data Cache = Cache {

     ,
    }

data Index = Index {
      iName         :: !Text
    , iSharedDirs   :: TVar (Map Text IndexedDirectory)
    }

data IndexedDirectory = IndexedDirectory {
      idPath    :: FilePath
    , idFiles   :: TVar (Map Text IndexedFile)
    }

data IndexedFile = IndexedFile {
      ifPath    :: FilePath
    , ifDesc    :: TVar (Maybe FileDesc)
    }

newIndex :: Text -> STM Index
newIndex name = Index name <$> newTVar M.empty

addSharedDirectory :: Index -> Text -> FilePath -> STM ()
addSharedDirectory index name path = do
    directory <- IndexedDirectory path <$> newTVar M.empty
    modifyTVar' (iSharedDirs index) (M.insert name directory)
