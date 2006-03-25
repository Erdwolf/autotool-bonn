{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

-- main module

import Network.XmlRpc.Client

server = "http://localhost/cgi-bin/IntegerServer.cgi"

main = do
     ( res :: Integer ) <- remote server "copy" ( 1234567 :: Integer )
     print res
