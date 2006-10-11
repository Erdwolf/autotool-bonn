{-# OPTIONS -fallow-overlapping-instances #-}

-- | test module: call some server methods

module Main where

import Network.XmlRpc.Client

import Modular.Documented
import Modular.Signed
import Modular.Task
import Modular.Config

type URL = String

server :: URL
server = "http://localhost/cgi-bin/modular-server.cgi"

list_types :: URL -> IO [ Task ]
list_types url = remote url "autotool.list_types"

get_config :: URL -> Task -> IO ( Documented Config )
get_config url = remote url "autotool.get_config"

verify_config :: URL -> Task -> Config -> IO ( Signed Config )
verify_config url = remote url "autotool.verify_config"

main :: IO ()
main = do
    ts <- list_types server
    print $ zip [0.. ] ts
    let task = ts !! 78

    dconf <- get_config server task
    print $ dconf
    let conf = Modular.Documented.contents dconf

    sconf <- verify_config server task conf
    print sconf


