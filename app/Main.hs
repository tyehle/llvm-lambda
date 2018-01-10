module Main where

import Data.ByteString.Char8 as BS

import Lib

main :: IO ()
main = toLLVM someIR >>= BS.putStrLn
