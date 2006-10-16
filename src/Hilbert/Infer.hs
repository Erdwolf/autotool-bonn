module Infer

( infer
)

where

import Set
import FiniteMap
import Sorters

import Ids
import Syntax

import Env
import Rules
import Sub

import Read

import Unify
import Match

import Assert

import Axioms

-----------------------------------------------------------------------

takefrom (x : xs) =
    (x, xs) : [ (y, x : ys) | (y, ys) <- takefrom xs ]
takefrom [] = []

------------------------------------------------------------------------

prefer :: [ Exp ] -> ( [ Exp ] , String )
-- testen, was sofort und ohne nebenwirkungen passt
prefer targets =
    let short t = take 1 -- der erste reicht
		  [ inf 
		  | rule @ ([], c) <- rules
		  , fm <- matchl c t
		  , let inf = "subgoal: " ++ show t
			    ++ ", solved by rule: " ++ show rule
			    ++ ", under subst: " ++ show fm
			    ++ ", generates no new subgoals"
		  ]

	tfms = [ (t, short t) | t <- targets ]
	infs = concat [ inf | (t, inf : _) <- tfms ]
	rest = [ t | (t, []) <- tfms ]
    in	( rest, infs )

-----------------------------------------------------------------------


infgoal :: Set Exp -> Exp -> [ ( Env, [ Exp ], String ) ]
infgoal vars t =
    do 
       rule @ (p, c) <- rules

       assert $ rule == modusponens || not (isvar c)

       let rule' @ (prems, conc) = rename rule vars

       fm <- unifyl conc t
       let prems' = [ apply fm p | p <- prems ]

       let inf = "subgoal: " ++ show t
	       ++ ", solved by rule: " ++ show rule' 
	       ++ ", under subst: " ++ show fm
	       ++ ", generates new subgoals: " ++ show prems'

       return $ (fm, prems', inf)

-----------------------------------------------------------------------


infer :: [ Exp ] -> [([ Exp ], String)]
infer targets = weed $
    do 
       let (todo, pref) = prefer targets
       let vars = unionManySets [ subvars t | t <- todo ]    

       (t, odo) <- takefrom todo

       (fm, prems', inf) <- infgoal vars t

       let rest = [ apply fm o | o <- odo ]
       let inf' = inf ++ ", still need to prove: " ++ show rest

       return $ (us (rest ++ prems'), pref ++ inf')

weed :: Ord a => [(a,b)] -> [(a,b)] 
weed  = fmToList . listToFM

-----------------------------------------------------------------------

subvars x = filterSet isvar $ subs x

rename (prems, conc) vars =
    let vs = unionManySets [ mapSet varname $ subvars t | t <- conc : prems ]
	supply = [ v 
		 | k <- [1..]
		 , let fname = "V" ++ show k
		 , let fid = usercon 0 fname
		 , let v = App fid []
		 , not $ v `elementOf` vars
		 ]
	fm = listToFM $ zip (setToList vs) supply
    in	( [ apply fm p | p <- prems ]
	, apply fm conc
	)

-----------------------------------------------------------------------







