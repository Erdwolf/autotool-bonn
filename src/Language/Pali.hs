module Language.Pali 

-- -- $Id$


( pali
, nopali
)

where

import Language.Type
import Util.Wort

import Set
import Random

pali :: Language
pali = Language
	{ abbreviation = "{ w | w in {0,1}^*  und  w = reverse w }"
	, nametag      = "Pali"
	, alphabet     = mkSet "01"
	, contains     = is_pali 
	, sample       = sam
	, anti_sample  = anti_sam
	}

is_pali w = w == reverse w

nopali :: Language
nopali = ( komplement pali )
	{ nametag      = "ComPali"
        , abbreviation = "{ w : w in {0,1}^*, w /= reverse w }"
	}

-------------------------------------------------------------------------

sam :: Int -> Int -> IO [ String ]
sam 0 n = return []
sam c 0 = return [ "" ]
sam c n = do
    let (q, r) = divMod n 2
    sequence $ replicate c $ do
        w <- someIO (setToList $ alphabet pali) (q + r)
	return $ w ++ drop r (reverse w)

anti_sam :: Int -> Int -> IO [ String ]
anti_sam 0 n = return []
anti_sam c n | n < 2 = return []
anti_sam c n = sequence $ replicate c $ do
    r <- randomRIO (n `div` 3, n `div` 2 - 1)

    let xs =  setToList $ alphabet pali
	l = length xs

    -- würfle zwei verschiedene Buchstaben x und y
    i <- randomRIO (0, l-1)
    d <- randomRIO (1, l-1)
    let x = xs !! i 
	y = xs !! ( (i+d) `mod` l )

    w <- someIO xs r
    v <- someIO xs (n-2)

    return $ w ++ [x] ++ v ++ [y] ++ reverse w

    
    










