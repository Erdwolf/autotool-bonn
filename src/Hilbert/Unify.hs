module Hilbert.Unify

( unify
, unifyl
)

where

-- import Syntax
import Autolib.FiniteMap
import Autolib.Set
-- import Assert

import Boolean.Op
import Expression.Op


import Hilbert.Env
import Sub

-------------------------------------------------------------------------

mbtl (Just x) = return x
mbtl Nothing = zero

unifyl :: Ord a => Exp a -> Exp a -> [ Env ( Exp a ) ]
unifyl l r = mbtl $ unify l r

unify :: Ord a => Exp a -> Exp a -> Maybe ( Env a )

unify pat exp | pat == exp = return Hilbert.Env.empty

unify pat exp | isvar pat =
    do
	assert $ not $ elementOf pat (subs exp)
	return $ listToFM [ (varname pat, exp) ]

unify exp pat | isvar pat = unify pat exp

unify (App pid pargs) (App xid xargs) =
    do assert $ pid == xid
       assert $ length pargs == length xargs
       unifyes (zip pargs xargs)


unifyes [] = return emptyFM
unifyes ((p, a) : qbs) =
  do
    fm <- unify p a
    let qbs' = [ (apply fm q, apply fm b) | (q, b) <- qbs ]
    fms <- unifyes qbs'
    let fm' = mapFM ( \ v w -> apply fms w ) fm
    return $ plusFM_C (error "unify") fm' fms


-------------------------------------------------------------------------

