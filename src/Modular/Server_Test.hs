{-# OPTIONS -fallow-overlapping-instances #-}

-- | test module: call some server methods

module Main where

import Network.XmlRpc.Client


type URL = String

server :: URL
server = "http://localhost/cgi-bin/modular-server.cgi"

list_types :: URL -> IO [ String ]
list_types url = remote url "autotool.list_types"

main :: IO ()
main = do
    ts <- list_types server
    print ts

