module Hilbert.Infer

( infer
, unvar
, distributes
)

where 

import Autolib.Set
import Autolib.TES.Term hiding ( unvar, assoc, precedence, arity )
import Autolib.FiniteMap
import Autolib.Util.Uniq


import Boolean.Op
import Expression.Op

import Hilbert.Proof

import Autolib.Util.Sort ( sort, nub, sortBy )
import Autolib.Size
import Autolib.Util.Hide

import Autolib.TES.Unify
import Autolib.TES.Identifier
import Data.Maybe
import Control.Monad ( guard )


-----------------------------------------------------------------------

takefrom (x : xs) =
    (x, xs) : [ (y, x : ys) | (y, ys) <- takefrom xs ]
takefrom [] = []


-----------------------------------------------------------------------


infgoal :: [ Rule ]
	-> Set Identifier
	-> Exp Bool 
	-> [ ( FiniteMap Identifier ( Exp Bool )
	     , [ Exp Bool ]
	     , Proof
	     ) 
	   ]
infgoal rules forbidden t = do 
       rule0 @ (prems0, conc0) <- rules
       let ( rule @ (prems, conc), sub ) = rename forbidden rule0 

       fm <- maybeToList $ mgu conc t
       let prems' = [ apply_partial fm p | p <- prems ]

       let inf = "subgoal: " ++ show t
	       ++ ", solved by rule: " ++ show rule
	       ++ ", under subst: " ++ show fm
	       ++ ", generates new subgoals: " ++ show prems'

       let orule = Local_Substitution sub 
		 $ Axiom { core =  rebuild rule0 }
	   proof = foldl Modus_Ponens orule $ map Reference prems'

       return $ (fm,  prems', proof)

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


-- | Inverse zu @distributes@
rebuild :: Rule -> Exp Bool
rebuild ( prems, conc ) =
    foldl ( \ x y -> Node implies [y, x] ) conc $ reverse prems


-----------------------------------------------------------------------

type Rule = ([Exp Bool], Exp Bool )

infer :: [Rule] -> [ Exp Bool ] 
      -> [ ( [ Exp Bool ]
	   , Hide (Exp Bool, Proof)
	   )
	 ]
infer rules targets = nub $    do 

       let (todo, pref) = ( targets, [] )

       let forbidden = unionManySets $ map vars todo

       -- consider only one subgoal
       (t, odo) <- take 1 
		 $ sortBy ( \ (t, _) -> negate $ size t ) 
		 $ takefrom todo

       (fm, prems', inf) <- infgoal rules forbidden t

       let rest = [ apply_partial fm o | o <- odo ]

       let ( forms, fmnorm ) = normalize  $ nub $ sort (rest ++ prems')

       return $ (   nub $ sort $ forms
		, Hide ( t, Global_Substitution fmnorm 
			  $ Global_Substitution fm inf )
		)

-- | rename variables
normalize forms = 
    let old = setToList $ unionManySets $ map vars forms
        new = do k <- [ 1 :: Int .. ] ; return $ mknullary $ "V" ++ show k
        fm = listToFM $ zip old new
    in  ( do f <- forms	      
	     return $ applyvar fm f
	, mapFM ( \ k v -> Var v ) fm
	)

weed :: Ord a => [(a,b)] -> [(a,b)] 
weed  = fmToList . listToFM

-----------------------------------------------------------------------

-- | Formel im Resultat hat disjunkte Variablen zu Menge
rename :: Set Identifier
       -> ( [Exp Bool], Exp Bool )
       -> ( ( [Exp Bool], Exp Bool )
	  , FiniteMap Identifier ( Exp Bool )
	  )
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
    in	( ( map ( applyvar fm ) prems , applyvar fm conc )
	, mapFM ( \ k v -> Var v ) fm
	)

-----------------------------------------------------------------------

unvar ( Node f args ) = Node f $ map unvar args
unvar ( Var v ) =  
    let op = Op { name = show v, arity = 0
		, precedence = Nothing , assoc = AssocNone
		, inter = undefined
		}
    in  Node op []







