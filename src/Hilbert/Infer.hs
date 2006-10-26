{-# OPTIONS -fglasgow-exts #-}

module Hilbert.Infer

( infer
, unvar
)

where 

import Autolib.Set
import Autolib.TES.Term hiding ( unvar, assoc, precedence, arity )
import Autolib.FiniteMap
import Autolib.Util.Uniq


import Boolean.Op
import Expression.Op

import Hilbert.Env


import Autolib.TES.Unify
import Autolib.TES.Identifier
import Hilbert.Axioms
import Data.Maybe
import Control.Monad ( guard )


rules = do
    ( name, form ) <- contents axioms
    ( prems, conc ) <- distributes form
    let pvars = unionManySets $ map vars prems
    return ( prems, conc )

-----------------------------------------------------------------------

takefrom (x : xs) =
    (x, xs) : [ (y, x : ys) | (y, ys) <- takefrom xs ]
takefrom [] = []

------------------------------------------------------------------------

-- | testen, was sofort und ohne nebenwirkungen passt
prefer :: [ Exp Bool ] -> ( [ Exp Bool ] , String )
prefer targets =
    let short t = take 1 $ do
	    ( [] , rule ) <- rules
	    fm <- maybeToList $ match rule t
	    return $ "subgoal: " ++ show t
			    ++ ", solved by rule: " ++ show rule
			    ++ ", under subst: " ++ show fm
			    ++ ", generates no new subgoals"

	tfms = [ (t, short t) | t <- targets ]
	infs = concat [ inf | (t, inf : _) <- tfms ]
	rest = [ t | (t, []) <- tfms ]
    in	( rest, infs )

-----------------------------------------------------------------------


infgoal :: Set Identifier
	-> Exp Bool 
	-> [ ( FiniteMap Identifier ( Exp Bool ), [ Exp Bool ], String ) ]
infgoal forbidden t = do 
       rule0 @ (prems, conc) <- rules
       let rule = rename forbidden rule0 

       fm <- maybeToList $ mgu conc t
       let prems' = [ apply_partial fm p | p <- prems ]

       let inf = "subgoal: " ++ show t
	       ++ ", solved by rule: " ++ show rule
	       ++ ", under subst: " ++ show fm
	       ++ ", generates new subgoals: " ++ show prems'

       return $ (fm, prems', inf)

-----------------------------------------------------------------------

-- | Axiom darstellen als ( Liste von Voraussetzungen, Folgerung )
-- Bsp: distribute  (A -> C) -> ((B -> C) -> (A || B -> C)) 
-- = ( [ A -> C, B -> C ] , A || B -> C )
distributes :: Exp Bool -> [ ( [ Exp Bool ], Exp Bool ) ]
distributes t = ( [] , t ) : case t of
    ( Node imp [ l, r ] ) | imp == implies -> do
        ( pre, con ) <- distributes r
        return ( l : pre, con )
    _ -> []

-----------------------------------------------------------------------

infer :: [ Exp Bool ] -> [([ Exp Bool ], String)]
infer targets = weed $    do 

       let (todo, pref) = ( targets, "" ) --  prefer targets

       let forbidden = unionManySets $ map vars todo

       (t, odo) <- takefrom todo

       (fm, prems', inf) <- infgoal forbidden t

       let rest = [ apply_partial fm o | o <- odo ]
       let inf' = inf ++ ", still need to prove: " ++ show rest

       return $ (uniq (rest ++ prems'), pref ++ inf')

weed :: Ord a => [(a,b)] -> [(a,b)] 
weed  = fmToList . listToFM

-----------------------------------------------------------------------

-- | Formel im Resultat hat disjunkte Variablen zu Menge
rename :: Set Identifier
       -> ( [Exp Bool], Exp Bool )
       -> ( [Exp Bool], Exp Bool )
rename forbidden ( prems, conc ) =
    let vs = do 
	    t <- conc : prems
	    setToList ( vars t )
	free = do 
	    k <- [ 1 :: Int .. ]
	    let v = mknullary $ "V" ++ show k
	    guard $ not $ elementOf v forbidden
	    return v
	fm :: FiniteMap Identifier Identifier
	fm = listToFM $ zip vs free
    in	( map ( applyvar fm ) prems
	, applyvar fm conc
	)

-----------------------------------------------------------------------

unvar ( Node f args ) = Node f $ map unvar args
unvar ( Var v ) =  
    let op = Op { name = show v, arity = 0
		, precedence = Nothing , assoc = AssocNone
		, inter = undefined
		}
    in  Node op []







