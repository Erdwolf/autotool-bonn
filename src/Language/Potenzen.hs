module Language.Potenzen

-- -- $Id$

where

import Language.Type
import Language.Zahl

import Autolib.Set
import System.Random

import Control.Monad ( guard )
import Data.Char

alpha = [ '0' .. '9' ]

wurzel :: Int -> Integer -> Integer
-- bestimmt abgerundete wurzel
wurzel e n | n <  0 = error "Language.Potenzen.wurzel: radikand <  0"
wurzel 1 n = n
wurzel e 0 = 0
wurzel e 1 = 1
wurzel e n | e <= 0 = error "Language.Potenzen.wurzel: exponent <= 0"
wurzel e n = 
    let c = 1
	ee = fromIntegral e
	f :: Integer -> Integer
	f x = (n + (ee-1) * x ^ e) `div` ( ee * x ^ (ee-1) )
	settle (x : y : ys) = if abs (x - y) <= 1 then x else settle (y : ys)
	d = settle $ iterate f c
    in	case ( do x <- reverse [ d - 2  .. d + 1 ]
		  let y = x ^ e
		  guard $ y <= n
		  return x )
	   of x : _ -> x
	      _	    -> error $ "wurzel" ++ show (e, n, c, d)


ist_wurzel :: Int -> Integer -> Bool
ist_wurzel e n = n == (wurzel e n) ^ e

potenzen :: Int -> Language
potenzen e =
   l where l = Language
	    { abbreviation = "{ dezimal(n^" ++ show e ++ ") : n >= 0 }"
	    , alphabet	   = mkSet alpha
	    , contains	   = \ w 
                  -> not ( null w ) 
	          && all isDigit w 
	          && ( ist_wurzel e (read w :: Integer) )
	    , sample       = \ c n -> sequence $ replicate c $ sam e n
            , anti_sample = sample $ komplement l
	    }

-- | w�rfelt ein Wort von ungef�hr passender L�nge
sam :: Int -> Int -> IO String
sam e l = do
    w <- zahl l
    let n = read w
    let x = wurzel e n
    return $ show $ x ^ e

