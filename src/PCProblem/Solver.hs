module PCProblem.Solver where

-- -- $Id$

import PCProblem.Type
import PCProblem.Konfig

import Schichten ( bfs )
import Data.Set
import Control.Monad ( guard )

start :: PCP -> Konfig
start i = Konfig 
        { instanz = i
	, folge = [] -- vorsicht: falschrum
	, tief = 0
	, oben = ""
	, unten = ""
	}

final :: Konfig -> Bool
final k = and [ not $ null $ folge k , null $ oben k , null $ unten k ]

dead :: Konfig -> Bool
dead k = and [ not $ null $ oben k, not $ null $ unten k ]

reduce :: Konfig -> Konfig
-- schneide gemeinsamen präfix (oben/unten) ab
reduce k = case ( oben k, unten k ) of
    ( x : xs, y : ys ) | x == y -> reduce $ k { oben = xs, unten = ys }
    _                           -> k

nachfolger :: Konfig -> [ Konfig ]
nachfolger k = do
    let PCP uvs = instanz k
    (i, (u, v))  <- zip [1..] uvs
    let k' = reduce 
	   $ k { folge = i : folge k
	       , tief = succ $ tief k
	       , oben = oben k ++ u
	       , unten = unten k ++ v 
	       }
    -- sonst nicht fortsetzbar
    guard $ not $ dead k'
    return k'

solutions :: Int -- maximal so viele knoten expandieren
	  -> Int -- lösungsfolgen maximal so lang
	   -> PCP 
	   -> [ Folge ]
-- der größe nach
solutions depth width i = do
    k0 <- nachfolger $ start i -- wenigstens einen schritt
    let fun k = mkSet $ do
           guard $ tief k < width
	   nachfolger k
    k <- take depth $ bfs fun k0
    guard $ final k
    return $ reverse $ folge k -- hier rumdrehen


