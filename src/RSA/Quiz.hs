{-# LANGUAGE MultiParamTypeClasses #-}
module RSA.Quiz 

( make
, Param (..)
, Config (..)
)

where

--  $Id$

import RSA.Param
import RSA.Break hiding ( make )
import RSA.Break.Data
import Faktor.Prim
import Faktor.Certify ( powmod )

import Autolib.Util.Zufall
-- import Autolib.Util.Wort
import Autolib.Util.Seed

import Util.Datei

import Inter.Types
import Inter.Quiz hiding ( make )

import Data.List (nub )

roll p = do
    let ps = dropWhile ( < fromIntegral ( von p ) ) 
	   $ takeWhile ( < fromIntegral ( bis p ) ) 
	   $ primes ( 100 :: Integer )
    [p, q] <- someDifferentIO ps 2
    let n = p * q
    let phi = pred p * pred q
    d <- coprime phi
    x <- coprime n
    return $ Config
	   { public_key = ( d, n )
	   , message = powmod x d n
	   }

-- | don't use for large n (stupid implementation)
someDifferentIO :: Eq a => [a] -> Int -> IO [a]
someDifferentIO xs n = someIO xs n 
    `repeat_until` all_different

all_different :: Eq a => [a] -> Bool
all_different xs = length xs == length (nub xs)
    

coprime :: Integer -> IO Integer
coprime n = randomRIO (1, n-1)
    `repeat_until` \ x -> 1 == gcd x n
 
instance Generator RSA_Code_Break Param Config where
    generator _ p key = roll p

instance Project RSA_Code_Break Config Config where
    project _ = id

make :: Make
make = quiz RSA_Code_Break RSA.Param.example


