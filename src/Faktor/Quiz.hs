module Faktor.Quiz where

--  $Id$

import Faktor.Param
import Faktor.Type
import Faktor.Prim

import Autolib.Util.Wort
import Inter.Quiz

-------------------------------------------------------------------------------

roll :: Param -> IO Integer
roll p = do
    let ps = dropWhile ( < fromIntegral ( von p ) ) 
	   $ takeWhile ( < fromIntegral ( bis p ) ) 
	   $ primes ( 1000 :: Integer )
    w <- someIO ps ( anzahl p )
    return $ product w

instance Generator Faktor Param ( Int , Integer ) where
    generator _ conf key = do
       zahl <- roll conf
       return ( anzahl conf , zahl )

instance Project Faktor ( Int , Integer ) ( Int , Integer ) where
    project _ = id


-------------------------------------------------------------------------------
-- import Autolib.Util.Seed

-- import Util.Datei
-- import Util.Cache

-- import Inter.Types

{-
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
             , tag = auf ++ "-" ++ ver
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

fixed :: String 
      -> String
      -> Integer
      -> IO Variant
fixed auf ver x = return $ Variant 
       $ Var { problem = Faktor
             , tag = auf ++ "-" ++ ver
             , key = \ mat -> return mat
             , gen = \ key -> do
                   return $ return x
	     }
-}
