module Faktor.Quiz 

( make
, Param (..)
)

where

--  $Id$

import Faktor.Param
import Faktor.Faktor
import Faktor.Prim

import Util.Wort
import Util.Seed
import Util.Datei
import Util.Cache

import Inter.Types

roll :: Param -> IO Integer
roll p = do
    let ps = dropWhile ( < fromIntegral ( von p ) ) 
	   $ takeWhile ( < fromIntegral ( bis p ) ) 
	   $ primes ( 317 :: Integer )
    w <- someIO ps ( anzahl p )
    return $ product w

make :: Param -> IO Variant
make p = return 
       $ Variant
       $ quiz "Faktor" "Quiz" p

quiz :: String -- Aufgabe
     -> String -- Version
     -> Param
     -> Var Faktor Integer (Integer, Integer)
quiz auf ver par =
         Var { problem = Faktor
             , aufgabe = auf
             , version = ver
             , key = \ mat -> return mat
             , gen = \ key -> do
                   seed $ read key
                   x <- cache (  Datei { pfad = [ "autotool", "cache"
                                           , auf, ver
                                           ]
                                  , name = key
                                  , extension = "cache"
                                  }
                         ) ( roll par )
                   return $ return x
	      }
