module Env 

( Env (..)
, look
, apply
, mkfm

, isvar, varname

, islower, isupper
)

where

import Ids
import Syntax
import FiniteMap

import Ops

type Env = FiniteMap Id Exp


----------------------------------------------------------------


isvar :: Exp -> Bool
isvar (App id []) = isupper $ head (idname id) 
isvar _ = False

isupper c = 'A' <= c && c <= 'Z'
islower c = 'a' <= c && c <= 'z'

varname :: Exp -> Id
varname (App id []) = id
varname x = error $ "varname: " ++ show x

--------------------------------------------------------------------

look :: Env -> Exp -> Exp
-- must be there
look env (App id [])  =
    case lookupFM env id of
	 Just x -> x
	 Nothing -> error $ "identifier " ++ show id 
			  ++ " not in environment " ++ show env

mkfm :: Exp -> Env
mkfm (Coll _ xs) = addListToFM_C (error "mkfm: clash") emptyFM 
		 [ mkpair x | x <- xs ]
mkfm foo = error $ "mkfm: " ++ show foo

mkpair (App fun [App id [], val]) | fun == assign = (id, val)
mkpair foo = error $ "mkpair: " ++ show foo

apply :: Env -> Exp -> Exp
apply env x @ (App id args) =
    if null args
    then case lookupFM env id of
	      Just y -> y
	      Nothing -> x
    else App id [ apply env arg | arg <- args ]

