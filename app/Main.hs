module Main where

import RpnServer (startRpnServer)
import System.Environment

main :: IO ()
main =
  do
    args <- getArgs
    let port = getPort args
    startRpnServer port
  where
    getPort :: [String] -> String
    getPort [] = "5001"
    getPort [x] = x