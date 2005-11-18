module Language.Vielfache

-- -- $Id$

where

import Language.Type
import Language.Zahl

import Autolib.Set
import System.Random
import Data.Char

alpha = [ '0' .. '9' ]

vielfache :: Integer -> Language
vielfache m = 
  l where l = Language
	    { abbreviation = "{ dezimal(n) : " ++ show m ++ " teilt n }"
	    , alphabet	   = mkSet alpha
	    , contains	   = \ w -> not ( null w ) &&
	          all isDigit w && ( 0 == (read w :: Integer) `rem` m )
	    , sample       = \ c n -> sequence $ replicate c $ sam m n
            , anti_sample = sample $ komplement l
	    }

sam :: Integer -> Int -> IO String
-- würfelt ein Wort von ungefähr passender Länge
sam m l = do
    w <- zahl l
    let n = read w
    let nn = n - rem n m 
    return $ show nn

