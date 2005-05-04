-- $Id$

-- | Bruchteilrucksack-Problem: Packe in einen Rucksack der Kapazität
-- | C (Teile von) Elemente(n), so dass das Gesamtgewicht nicht über C
-- | liegt und der Gesamtwert maximiert wird. => Greedy funktioniert.

module Knapsack.Solve where

import Data.Ratio ( Ratio , (%) )
import Data.List ( sortBy , groupBy , nubBy )

import Autolib.Pick ( permutations )

-------------------------------------------------------------------------------

-- | gibt *alle* optimalen packungen zurück
-- | arg 1: elemente, die zu packen sind
-- | arg 2: kapazität des rucksacks
-- | arg 3: gewicht der elemente
-- | arg 4: wert der elemente

-- | zurück: (gesamtwert,packung)

packs :: Integral b 
      => [a] -> b -> (a -> b) -> (a -> b) 
      -> [(Ratio b,[(a,Ratio b)])]
packs xs c g v = 
    let gs            =  map g xs
	vs            =  map v xs
        lift1 fun x y =  fun (fst y) (fst x)
	rss           =  map (map snd)
		      $  all_sorts (lift1 (==))
		      $  sortBy (lift1 compare) 
		      $  zip (zipWith (%) vs gs) (zip (zip [(0::Int)..] xs) gs)
        lift2 x y     =  compare (fst $ fst x) (fst $ fst y)
        the_packs     =  map (pack c) rss
	resorted      =  map (sortBy lift2) the_packs
	prj ((_,x),r) =  (x,r)
	lift3 x y     =  (==) (map snd x) (map snd y)
        count f xrs   =  sum $ do (x,r) <- xrs ; return $ r * (f x % 1)
    in do xrs <- nubBy lift3 $ map ( map prj ) resorted
	  return ( count v xrs , xrs )

-------------------------------------------------------------------------------

pack :: Integral b => b -> [(t,b)] -> [(t,Ratio b)]
pack _ []             = []
pack 0 xgs            = do (x,_) <- xgs; return (x,0)
pack c ((x,g):rest) 
    | g <= c          = (x,1    ) : pack (c-g) rest
pack c ((x,g):rest)   = (x,c % g) : pack    0  rest

-------------------------------------------------------------------------------

all_sorts :: (a -> a -> Bool) -> [a] -> [[a]]
all_sorts eq = emit . groupBy eq where emit []     = [[]]
				       emit (g:gs) = do p <- permutations g
							e <- emit gs
							return $ p ++ e
