module WWRW 

-- -- $Id$


( wwrw
)

where


import Data.Set
import Data.List ( nub )
import Random

import Wort
import Language

wwrw :: Language
wwrw = Language
	      { abbreviation = "{ w w^R w : w in {0,1}^* }"
	      , alphabet     = mkSet "01"
	      , contains     = ok
	      , sample       = sam
	      }

ok :: String -> Bool
ok w = 
    let (q, r) = divMod (length w) 3
	(a, bc) = splitAt q w
	(b, c) = splitAt q bc
    in	0 == r && a == c && a == reverse b

sam :: Int -> Int -> IO [ String ]
sam c n | 0 /= n `mod` 3 = return []
sam c n = do
    let q = n `div` 3
    ws <- sequence $ replicate c $ do
       w <- someIO (setToList $ alphabet wwrw) q
       return $ w ++ reverse w ++ w
    return $ nub ws

    





