module Match

( match
, matchl
)

where

import Syntax
import FiniteMap
import Assert

import Env

-------------------------------------------------------------------------

mbtl (Just x) = return x
mbtl Nothing = zero

matchl :: Exp -> Exp -> [ Env ]
matchl l r = mbtl $ match l r

match :: Exp -> Exp -> Maybe Env
-- bind variables in first arg
match pat exp | isvar pat =
    return $ listToFM [ (varname pat, exp) ]

match (App pid pargs) (App xid xargs) =
    do assert $ pid == xid
       assert $ length pargs == length xargs
       matches (zip pargs xargs)

same fm1 fm2 =
     do
	let fm = intersectFM_C (\ l r -> (l, r)) fm1 fm2
	assert $ and [ l == r | (l, r) <- eltsFM fm ]
	return $ plusFM fm1 fm2


matches [] = return emptyFM
matches ((p, a) : qbs) =
  do
    fm <- match p a
    let qbs' = [ (q, apply fm b) | (q, b) <- qbs ]
    fms <- matches qbs'
    same fm fms

-------------------------------------------------------------------------

