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

module Main where

import ClassyPrelude

import qualified Data.ByteString.Lazy as BL

import Hannibal.Pieces (fileDesc)

main :: IO ()
main = do
    desc <- fileDesc <$> BL.readFile "/home/rapha/big.iso"
    print desc
