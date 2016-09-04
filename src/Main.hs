module Main where

import ClassyPrelude

import qualified Data.ByteString.Lazy as BL

import Hannibal.Pieces (fileDesc)

main :: IO ()
main = do
    desc <- fileDesc <$> BL.readFile "/home/rapha/big.iso"
    print desc
