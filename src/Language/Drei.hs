module Language.Vielfache

where

import Language.Type

import Random

alpha = [ '0' .. '9' ]

vielfache :: Integer -> Language
vielfache m = Language
	    { abbreviation = "{ dezimal(n) : " ++ show m ++ " teilt n }"
	    , alphabet	   = mkSet alpha
	    , contains	   = \ w -> 
	          all isDigit w && ( 0 == (read w :: Integer) `rem` m )
	    , sample       = \ m c n -> sequence $ replicate c $ sam m n
	    }

zahl :: Int -> IO Integer
-- ein Integer dieser Länge
zahl 0 = return 0
zahl l = do
     c <- randomRIO [ '1' .. '9' ]
     cs <- sequence $ replicate l $ randomRIO alpha
     return $ read ( c : cs )

sam :: Integer -> Int -> IO String
-- würfelt ein Wort von ungefähr passender Länge
sam m l = do
    n <- zahl l
    let nn = n - rem n m 
    return $ show n

