module Language.Gleich 

-- -- $Id$


( gleich 
, ordered_gleich
, ordered_ungleich

, edits, edit
) 

-- TODO: allgemeineres interface schreiben für
-- { a^i b^j c^k | p(i,j,k) }
-- für irgendwelche prädikate p

where

import Language.Type
import Language.Genau

import Util.Edit
import Util.Uniq

import Sets
import Data.List (intersperse, nub, group, sort)
import Random
import Control.Monad

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

blocks :: Eq a => [a] -> [a] -> Maybe [[a]]
blocks [] w = do
    guard $ null w
    return []
blocks (x : xs) w = do
    let (pre, post) = span (== x) w
    rest <- blocks xs post
    return ( pre : rest )

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
    case blocks xs w of
	 Nothing -> False
	 Just gs -> 
	     let ls = map length gs
	     in  all ( uncurry ( == ) ) $ zip ls $ tail ls 

ordered_gleich_sam :: String -> Int -> Int -> IO [ String ]
-- liefert evtl. etwas kürzere Wörter
ordered_gleich_sam xs c n = 
    let ( d, m ) = divMod n ( length xs )
    in  return [ do x <- xs ; replicate d x ]

-----------------------------------------------------------------------------

ordered_ungleich :: String -> Language
ordered_ungleich xs = Language 
       { nametag = "OUnGleich"
       , abbreviation = 
           let is = do i <- take (length xs) [ 'i' .. ] ; return [ i ]
	       here = concat $ intersperse " " $ do
	                 (x , i ) <- zip xs is
	                 return $ [x] ++ "^" ++ i 
	       there = concat $ intersperse " oder " $ do
	                 (i, j) <- zip is $ tail is
	                 return $ i ++ " /= "++ j
           in  "{ " ++ here ++ " | " ++ there ++ " }"
       , alphabet     = mkSet xs
       , sample       = ordered_ungleich_sam xs
       , anti_sample  = \ c n -> do
	    -- die sind überhaupt durcheinander (ganz kaputt)
	    ws <- anti (ordered_ungleich_sam xs) (ordered_ungleich_con xs) c n
	    -- die sind zu sehr in ordnung (alle blöcke gleichlang)
	    us <- ordered_gleich_sam xs c n
	    return $ us ++ ws
       , contains     = ordered_ungleich_con xs
       }


ordered_ungleich_con :: String -> String -> Bool
ordered_ungleich_con xs w = 
    case blocks xs w of
	 Nothing -> False
	 Just gs -> 
	     let ls = map length gs
	     in  not $ all ( uncurry ( == ) ) $ zip ls $ tail ls 

ordered_ungleich_sam :: String -> Int -> Int -> IO [ String ]
ordered_ungleich_sam xs c n = do
    -- c : (maximale) Anzahl 
    -- n : Wortlänge
    ws <- ordered_gleich_sam xs c n 
    us <- mapM edits $ concat $ replicate ( 4 * c ) ws
    return $ uniq $ filter ( ordered_ungleich_con xs ) $ us

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
	



    







