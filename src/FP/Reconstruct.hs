module FP.Reconstruct where

import FP.Type
import FP.Arrow
import FP.Expression
import FP.Env

import Autolib.TES.Unify
import Autolib.TES.Identifier
import Autolib.TES.Term
import Autolib.TES.Position
import Autolib.Symbol

import Autolib.FiniteMap
import Autolib.Set
import Autolib.Reporter hiding ( wrap  )

import Autolib.ToDoc

import Data.Char


env0 :: Env
env0 = Env $ listToFM 
    [ ( read "map", read "forall a b . (a -> b) -> [a] -> [b]" )
    , ( read "append", read "forall a . [a] -> [a] -> [a]" )
    , ( read "reverse", read "forall a. [a] -> [a]" )
    , ( read "nil", read "forall a. [a]" )
    , ( read "c", read "forall a b c . (a -> b) -> (b -> c) -> (a -> c)" )
    ]

look :: Env -> Identifier -> Reporter Type
look ( Env env ) at = do
    case lookupFM env at of
        Nothing -> reject $ vcat 
			  [ text "keine Typinformation über" <+> toDoc at
			  ]
	Just t -> return t

reconstruct ::  Env
	    -> Expression Identifier
	    -> Reporter Type
reconstruct env x = do
    t0 <- reconstructor env x
    return $ wrap t0

reconstructor env x = do
    inform $ vcat
	   [ text "berechne allgemeinsten Typ für"
	   , nest 4 $ toDoc x
	   ]
    t0 <- nested 6 $ reconstruct0 env x
    inform $ vcat 
	   [ text "allgemeinster Typ für", nest 4 $ toDoc x
	   , text "ist  ", nest 4 $ toDoc ( wrap t0 )
	   ]
    return t0
    

reconstruct0 :: Env
	    -> Expression Identifier
	    -> Reporter ( Arrow Identifier )
reconstruct0 env ( Atomic at ) = do
    t <- look env at    
    return $ core t
reconstruct0 env ( Apply fun arg ) = do
    tfun <- reconstructor env fun
    targ <- reconstructor env arg

    combine (fun, tfun) ( arg, targ)

combine :: ( Expression Identifier, Arrow Identifier )
	-> ( Expression Identifier, Arrow Identifier )
	->  Reporter ( Arrow Identifier )
combine (fun,tfun) (arg,targ) = do
    let [ src, tgt ] = unused' 2 $ vars $ unArrow tfun
    let ar = Node ( mkunary "Arrow" ) [ Var src, Var tgt ]
    inform $ text "soll ein Funktionstyp sein:" <+> toDoc ( Arrow ar )

    sigma <- case mgu ar $ unArrow tfun of
        Just sigma -> do
	   inform $ vcat [ text "paßt mit Substitution"
			 , nest 4 $ toDoc ( Sub sigma )
			 ]
	   return sigma
	Nothing -> reject $ text "Falsch"
    let src1 = apply_partial sigma $ Var src
	tgt1 = apply_partial sigma $ Var tgt

    let targ1 = Arrow $ disjoint_renamed ( vars $ unArrow tfun ) $ unArrow targ 
    inform $ vcat [ text "umbenannt in", nest 4 $ toDoc targ1 ]

    inform $ vcat [ text "soll passen zum Argumenttyp"
		  , nest 4 $ toDoc ( Arrow src1 )
		  ]
    rho <- case mgu ( unArrow targ1 ) src1 of
        Just rho -> do
	    inform $ vcat 
		   [ text "paßt mit Substitution", nest 4 $ toDoc ( Sub rho ) ]
	    return rho
	Nothing -> reject $ text "Falsch"

    return $ Arrow $ apply_partial rho tgt1
    

isinstanceof tgt typ = do
    inform $ vcat
	   [ text "Typ", nest 4 $ toDoc typ
	   , text "ist allgemeiner als Typ", nest 4 $ toDoc tgt
	   ]
    case match ( unArrow typ ) ( unArrow tgt ) of
	 Just sub -> do
	     inform $ vcat 
		    [ text "paßt mit Substitution"
		    , nest 4 $ toDoc ( Sub sub )
		    ]
	     return sub
	 Nothing -> do 
	     reject $ text "paßt nicht."

data Sub = Sub ( FiniteMap Identifier ( Term Identifier Identifier ) )

instance ToDoc Sub where
    toDoc ( Sub sub ) = dutch Nothing ( text "{" , semi , text "}" ) $ do
       ( n, t ) <- fmToList sub
       return $ toDoc n <+> text "=" <+> toDoc ( Arrow t )

disjoint_renamed vs t = 
    let ws = vars t
        common = setToList $ intersect vs ws
        new = unused' ( length common ) ( union vs ws )
	rename = listToFM $ zip common $ map Var new
    in  apply_partial rename t

unused' k vs = take k $ do 
    v <- pool
    guard $ not $ v `elementOf` vs
    guard $ all isAlphaNum $ show v
    return v
