module RSA.Quiz 

( make
, fixed
, Param (..)
, Config (..)
)

where

--  $Id$

import RSA.Param
import RSA.Break
import RSA.Break.Data
import Faktor.Prim
import Faktor.Certify ( powmod )

import Autolib.Util.Zufall
import Autolib.Util.Wort
import Autolib.Util.Seed

import Util.Datei
import Util.Cache

import Random

import Inter.Types

roll :: Param -> IO Config
roll p = do
    let ps = dropWhile ( < fromIntegral ( von p ) ) 
	   $ takeWhile ( < fromIntegral ( bis p ) ) 
	   $ primes ( 100 :: Integer )
    [p, q] <- someIO ps 2
    let n = p * q
    let phi = pred p * pred q
    d <- coprime phi
    x <- coprime n
    return $ Config
	   { public_key = ( d, n )
	   , message = powmod x d n
	   }

coprime :: Integer -> IO Integer
coprime n = randomRIO (1, n-1)
    `repeat_until` \ x -> 1 == gcd x n
 

make :: Param -> IO Variant
make p = return 
       $ Variant
       $ quiz "Break" "Quiz" p

quiz :: String -- Aufgabe
     -> String -- Version
     -> Param
     -> Var Break Config Integer
quiz auf ver par =
         Var { problem = Break
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
      -> Config
      -> IO Variant
fixed auf ver x = return $ Variant 
       $ Var { problem = Break
             , tag = auf ++ "-" ++ ver
             , key = \ mat -> return mat
             , gen = \ key -> do
                   return $ return x
	     }
