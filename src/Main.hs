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

module Main where

import ClassyPrelude

import Hannibal.Config (defaultConfig)
import Hannibal.Instance (getInstance, runWithInstance)
import Hannibal.Network.Discovery (announceInstance, discoveryDaemon)

-- import Hannibal.Filesystem.FileIndex (newIndex, addDirectory)

main :: IO ()
main = do
    -- idx <- pure (newIndex "Raphael's MacBook") >>=
    --     addDirectory "Desktop" "/Users/rapha/Desktop" >>=
    --     addDirectory "Hannibal" "/Users/rapha/hannibal"
    --
    -- print idx

    inst <- getInstance defaultConfig

    _ <- runWithInstance discoveryDaemon inst

    forever $ do
        runWithInstance announceInstance inst
        threadDelay 10000000
