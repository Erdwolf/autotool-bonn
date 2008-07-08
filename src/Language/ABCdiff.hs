{-# OPTIONS -fglasgow-exts #-}

module Language.ABCdiff (abcdiff) where

-- -- $Id$

import Language

import Autolib.Set
import Autolib.Util.Zufall

import Control.Monad ( guard )
import Data.List ( sort, nub )


abcdiff :: Language
abcdiff = Language 
       { abbreviation = "{ a^i b^j c^k : i /= j, j /= k, k /= i }" 
       , nametag      = "ABCdiff"
       , alphabet     = mkSet "abc"
       , sample       = sam
       , anti_sample  = error "ABCdiff.anti_sample not implemented"
       , contains     = con
       }

sam :: RandomC m
    => Int -> Int 
    -> m [ String ]
sam c n = do
    ws <- sequence $ replicate c $ do
	  w <- sequence $ replicate n $ randomRIO ('a', 'c')
	  return $ sort w
    return $ filter (contains abcdiff) $ nub ws

con :: String -> Bool
con w0 = 
    let (as, w1) = span (== 'a') w0; i = length as
	(bs, w2) = span (== 'b') w1; j = length bs
	(cs, w3) = span (== 'c') w2; k = length cs
    in	null w3 && (i /= j) && (j /= k) && (k /= i)








