module Language.Vielfache

-- -- $Id$

where

import Language.Type
import Language.Zahl

import Set
import Random
import Char

alpha = [ '0' .. '9' ]

vielfache :: Integer -> Language
vielfache m = Language
	    { abbreviation = "{ dezimal(n) : " ++ show m ++ " teilt n }"
	    , alphabet	   = mkSet alpha
	    , contains	   = \ w -> not ( null w ) &&
	          all isDigit w && ( 0 == (read w :: Integer) `rem` m )
	    , sample       = \ c n -> sequence $ replicate c $ sam m n
	    }

sam :: Integer -> Int -> IO String
-- w�rfelt ein Wort von ungef�hr passender L�nge
sam m l = do
    w <- zahl l
    let n = read w
    let nn = n - rem n m 
    return $ show nn

