module Language.Gleich 

-- $Id$


( gleich 
, ordered_gleich
) 

-- TODO: allgemeineres interface schreiben für
-- { a^i b^j c^k | p(i,j,k) }
-- für irgendwelche prädikate p

where

import Language.Type
import Language.Genau

import Util.Edit

import Sets
import List (intersperse, nub, group, sort)
import Random

-----------------------------------------------------------------------------

gleich :: String -> Language
gleich xs = Language 
       { nametag = "Gleich"
       , abbreviation = "{ w : " ++ concat ( intersperse " = "
			[ "|w|_" ++ [x] | x <- xs ] ) ++ " }"
       , alphabet     = mkSet xs
       , sample       = gleich_sam xs
       , anti_sample  = anti (gleich_sam xs) (gleich_con xs)
       , contains     = gleich_con xs
       }

gleich_sam :: String -> Int -> Int -> IO [ String ]
gleich_sam xs c n = 
    let (q, r) = divMod n (length xs)
    in	if 0 == r
	then do ws <- sequence $ replicate c $ genau [ (x, q) | x <- xs ]
		return $ nub ws
	else return []

gleich_con :: String -> String -> Bool
gleich_con xs w = 
    let count x = length ( filter (== x) w )
	c : cs = map count xs
    in	all (== c) cs

-----------------------------------------------------------------------------

ordered_gleich :: String -> Language
ordered_gleich xs = Language 
       { nametag = "OGleich"
       , abbreviation = 
           let is = do i <- take (length xs) [ 'i' .. ] ; return [ i ]
	       here = concat $ intersperse " " $ do
	                 (x , i ) <- zip xs is
	                 return $ [x] ++ "^" ++ i 
	       there = concat $ intersperse " = " $ is
           in  "{ " ++ here ++ " | " ++ there ++ " }"
       , alphabet     = mkSet xs
       , sample       = ordered_gleich_sam xs
       , anti_sample  = anti (ordered_gleich_sam xs) (ordered_gleich_con xs)
       , contains     = ordered_gleich_con xs
       }

ordered_gleich_con :: String -> String -> Bool
ordered_gleich_con xs w = 
    let gs = group w
        ordered = and $ do ( x, g ) <- zip xs gs
			   return $ not ( null g ) && x == head g
        ls = map length gs
	counted = all ( uncurry ( == ) ) $ zip ls $ tail ls
    in  ordered && counted

ordered_gleich_sam :: String -> Int -> Int -> IO [ String ]
ordered_gleich_sam xs c n = do
    ws <- gleich_sam xs c n
    return $ nub $ map sort ws

-----------------------------------------------------------------------------

-- todo: move to a generic place
anti :: ( Int -> Int -> IO [ String ] ) 
     -> ( String -> Bool )
     -> Int -> Int -> IO [ String ]
anti sam con c n = do
    ws <- sam c n
    vs <- sequence $ do
        w <- ws
	return $ edits w
    return $ filter ( not . con ) vs
	



    







