module Language.Gleich 

-- $Id$


( gleich 
) 

where

import Language.Type
import Language.Genau

import Util.Edit

import Sets
import List (intersperse, nub)
import Random


gleich :: String -> Language
gleich xs = Language 
       { nametag = "Gleich"
       , abbreviation = "{ w : " ++ concat ( intersperse " = "
			[ "|w|_" ++ [x] | x <- xs ] ) ++ " }"
       , alphabet     = mkSet xs
       , sample       = sam xs
       , anti_sample  = antisam xs
       , contains     = con xs
       }

sam :: String -> Int -> Int -> IO [ String ]
sam xs c n = 
    let (q, r) = divMod n (length xs)
    in	if 0 == r
	then do ws <- sequence $ replicate c $ genau [ (x, q) | x <- xs ]
		return $ nub ws
	else return []

antisam ::  String -> Int -> Int -> IO [ String ]
antisam xs c n = do
    ws <- sam xs c n
    vs <- sequence $ do
        w <- ws
	return $ edits w
    return $ filter ( not . con xs ) vs
	
con :: String -> String -> Bool
con xs w = 
    let count x = length ( filter (== x) w )
	c : cs = map count xs
    in	all (== c) cs



    







