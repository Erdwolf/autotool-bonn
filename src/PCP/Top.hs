{-# OPTIONS -fallow-overlapping-instances #-}

--  $Id$

module PCP.Top where

import PCP.Type
import PCP.Form
import PCP.Solve
import PCP.Examples
import PCP.Paths
import Autolib.Dot.Dot (display)

import Autolib.Util.Wort
import Autolib.Util.Sort
import Autolib.ToDoc
import Autolib.Hash
import Control.Monad
import Data.FiniteMap
import Autolib.Set



find f = do
    n' <- [ 0 .. ]
    w' <- alle "01" n'
    let w = "0" ++ w' ++ "1"
    let n = length w
	p = form w
    guard $ p <= spiegel p
    let ss = take 1 $ do
           ff <- [ 1 .. f ]
	   -- forback w ff
           spread w ff
    return (p, map length ss, ss)

pp w c =
    let inf = [ repeat ("w", "0")
	      , repeat ("0","1")
	      , repeat ("1", "w")
	      ]
        ffin = [ replicate c (w, "0") , [] , [] ]
        bfin = [ [] , [] , replicate c ("1", w) ]
    in  ( inf ++ ffin, inf ++ bfin )






forback w c = do
    let (for, back) = pp w c
    (d, s) <- tree for
    guard $ not $ null d
    (e, t) <- tree_from back d
    -- guard $ null e
    let down = map (`mod` 3)
    return (e, down $ reverse s ++ reverse t)

spread w c = do
    let (for, back) = pp w c
    let dss = do { (d, s) <- tree for; return (d, []) }
        ets = do { (e, t) <- tree $ map turn back ; return (reverse e, []) }
    (d, s, t) <- overlaps dss ets
    guard $ not $ null d
    let down = map (`mod` 3)
    -- return $ down $ reverse s ++ t    
    return d

overlaps xs ys = olaps xs ys emptyFM emptyFM True
olaps ((x,v) : xvs) yws xdone ydone orient = 
      let xnext = addToFM xdone (hash x, x) v
          hits = case lookupFM ydone (hash x, x) of
	              Just w -> if orient then [(x, v, w)] else [(x, w, v)]
                      Nothing -> []
      in  hits ++ olaps yws xvs ydone xnext (not orient)
olaps _ _ _ _ _ = []

--------------------------------------------------------


expand w cs = do
    c <- cs
    if c == 'w' then w else [c]

collect :: Ord c 
        => [[c]]
        -> FiniteMap Int (Set [c])
collect ws = addListToFM_C union emptyFM $ do
	 w <- ws
         return ( length w, unitSet w )

   
