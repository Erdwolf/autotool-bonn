module Sat

( sat
)

where


import FiniteMap
import Set

import Ids
import Syntax

import Ops
import Sub
import Env

-- teste, ob formel gültig/erfüllbar



exists = usercon 2 "exists" 
forall = usercon 2 "forall"

------------------------------------------------------------

mkcon :: [ Exp ] -> Exp
-- mache großes und
mkcon = foldl1 (\ l r -> App und [l, r])

cvs :: Exp -> ( Set Id, Set Id )
cvs t =
    let cs = mkSet [ fun | App fun [] <- setToList $ subs t ]
	exs = filterSet ( isupper . head . idname ) cs
	alls = filterSet ( islower . head . idname ) cs
    in (exs, alls)

mkquant :: Id -> Id -> Exp -> Exp
mkquant q id x = App q [ App id [], x ]

mkprefix q ids x = foldr ( \ id r -> mkquant q id r ) x (setToList ids)

--------------------------------------------------------

sat :: [ Exp ] -> Bool
sat xs = 
    let x = mkcon xs
	(exs, alls) = cvs x
	f = mkprefix forall alls . mkprefix exists exs $ x
    in	qbf emptyFM f


--------------------------------------------------------

qbf :: FiniteMap Id Bool -> Exp -> Bool

qbf fm (App fun []) =
    case lookupFM fm fun of
	 Just v -> v
	 Nothing -> error $ "qbf: not bound: " ++ show fun

qbf fm (App fun [arg ]) | fun == nicht = not (qbf fm arg)

qbf fm e @ (App fun [x, y]) =
    let vx = qbf fm x; vy = qbf fm y
    in	if fun == und then vx && vy
	else if fun == oder then vx || vy
	else if fun == implies then vx <= vy
	else if fun == equiv then vx == vy

	else if fun == exists then qbfq fm x or  y
	else if fun == forall then qbfq fm x and y

	else	error $ "qbf: strange expn: " ++ show e

qbf fm e = error $ "qbf: strange expn: " ++ show e


qbfq fm (App id []) op f =
    op [ qbf fm' f
       | v <- [ False, True ]
       , let fm' = addListToFM_C (error $ "qbfq: already bound" ++ show id)
	         fm [ ( id, v) ]
       ]

--------------------------------------------------------

    
