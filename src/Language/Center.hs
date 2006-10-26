module Language.Center ( center ) where

--  $Id$

import Language.Type

import Autolib.Set ( mkSet )

center :: String -> Char -> Language
center sigma c
    = let l = Language 
	    { nametag = "Center"
	    , abbreviation = foldl1 (++) 
	                   [ "{ w : w = x" , [c] , "y  und  |x|=|y| }" ]
	    , alphabet = mkSet sigma
	    , contains = element sigma c
	    , sample = random_sample l
	    , anti_sample = random_sample (komplement l)
	    }
      in l

element :: String -> Char -> String -> Bool
element sigma c w 
    = let (q,r) = divMod (length w) 2
          (_,x:_) = splitAt q w
      in and [ r == 1 , x == c ]
