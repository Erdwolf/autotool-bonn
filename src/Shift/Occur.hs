module Shift.Occur where

import Shift.Type
import Shift.Computer
import Shift.Common ( up )

import Set
import FiniteMap
import List (inits, tails)
import Monad (guard)
import ToDoc

type Item a = ( Int, [a], Maybe Int )
-- ( i, w, Nothing ) : teilwort w tritt an position i 
--   zum ersten mal (seit letztem reset) auf
-- ( i, w, Just d  ) : teilwort w an position i war schonmal d schritte vorher


jumps :: Ord a => Int -> [a] -> [ ( Int, ( Int, [a]) ) ]
jumps m w = up $ do 
      ( i, u, Just d ) <- occur m w
      return ( d, ( i, u) )

occur :: Ord a => Int -> [a] -> [ Item a ]
occur m w = filter ( \ ( i, u, j ) -> case j of Nothing -> True
						Just k -> k > 0 )
	  $ occ m (zip [0..] $ tails w) emptyFM

bis p [] = []
bis p (x : xs) = x : if p x then [] else bis p xs

ekat n = reverse . take n . reverse

neu ( i, u, j ) = j == Nothing

occ m [] fm = []
occ m ((i, w): rest) fm = 
    let
	its = do
	    u <- inits $ take m w
	    case lookupFM fm u of
	        Nothing -> return ( i, u, Nothing )
		Just j	-> return ( i, u, Just ( i - j ) )
        (here, there) = span (not . neu) its
	fm' = addListToFM fm $ do ( i, u, j ) <- its; return (u, i)
    in	(ekat 1 here ++ take 1 there) 
	++ occ m rest fm'
     
