{-# OPTIONS -fallow-overlapping-instances #-}

-- | test module: call some server methods

module Main where

import Network.XmlRpc.Client

import Modular.Documented
import Modular.Signed
import Modular.Task
import Modular.Config
import Modular.Seed
import Modular.Instance
import Modular.Solution
import Modular.Pair

type URL = String

server :: URL
server = "http://dfa.imn.htwk-leipzig.de/cgi-bin/modular-server.cgi"

list_types :: URL -> IO [ Task ]
list_types url = remote url "autotool.list_types"

get_config :: URL -> Task -> IO ( Documented Config )
get_config url = remote url "autotool.get_config"

verify_config :: URL -> Task -> Config -> IO ( Signed Config )
verify_config url = remote url "autotool.verify_config"

get_instance :: URL 
             -> Task
	     -> Signed Config
             -> Seed
	     -> IO ( Pair ( Documented ( Signed Instance ) ) 
                          ( Documented Solution )
                   )
get_instance url = remote url "autotool.get_instance"

grade :: URL
      -> Task
      -> Signed Instance
      -> Solution
      -> IO ( Documented ( Pair Bool Double ) )
grade url = remote url "autotool.grade"

main :: IO ()
main = do
    ts <- list_types server
    print $ zip [0.. ] ts
    let task = ts !! 79

    dconf <- get_config server task
    print $ dconf
    let conf = Modular.Documented.contents dconf

    sconf <- verify_config server task conf
    print sconf

    p <- get_instance server task sconf ( Seed 271828 )
    print p

    let sint = Modular.Documented.contents $ first p
        sol  = Modular.Documented.contents $ second p
	sol' = Solution "[ 23,34,45 ]"
    w <- grade server task sint sol'
    print w

        

    