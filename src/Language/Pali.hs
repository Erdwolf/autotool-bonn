module Language.Pali 

-- -- $Id$


( pali
, nopali
)

where

import Language.Type
import Autolib.Util.Wort

import Autolib.Set
import System.Random
import Data.List (intersperse)

pali :: String -> Language
pali sigma = 
        Language
	{ abbreviation = foldl1 (++) [ "{ w | w in {" 
				     , intersperse ',' sigma
				     , "}^*  und  w = reverse w }"
				     ]
	, nametag      = "Pali"
	, alphabet     = mkSet sigma
	, contains     = is_pali 
	, sample       = sam sigma
	, anti_sample  = anti_sam sigma
	}

is_pali w = w == reverse w

nopali :: String -> Language
nopali sigma = ( komplement $ pali sigma )
	{ nametag      = "ComPali"
	, abbreviation = foldl1 (++) [ "{ w | w in {" 
				     , intersperse ',' sigma
				     , "}^*  und  w = reverse w }"
				     ]
	}

-------------------------------------------------------------------------

sam :: String -> Int -> Int -> IO [ String ]
sam _ 0 n = return []
sam _ c 0 = return [ [] ]
sam sigma c n = do
    let (q, r) = divMod n 2
    sequence $ replicate c $ do
        w <- someIO sigma (q + r)
	return $ w ++ drop r (reverse w)

anti_sam :: String -> Int -> Int -> IO [ String ]
anti_sam _ 0 n = return []
anti_sam _ c n | n < 2 = return []
anti_sam sigma c n = sequence $ replicate c $ do
    r <- randomRIO (n `div` 3, n `div` 2 - 1)

    let l = length sigma

    -- würfle zwei verschiedene Buchstaben x und y
    i <- randomRIO (0, l-1)
    d <- randomRIO (1, l-1)
    let x = sigma !! i 
	y = sigma !! ( (i+d) `mod` l )

    w <- someIO sigma r
    v <- someIO sigma (n-2)

    return $ w ++ [x] ++ v ++ [y] ++ reverse w

    
    










