module FP.Reconstruct where

import FP.Type
import FP.Arrow
import FP.Expression

import Autolib.TES.Unify
import Autolib.TES.Identifier
import Autolib.TES.Term
import Autolib.TES.Position
import Autolib.Symbol

import Autolib.FiniteMap
import Autolib.Reporter hiding ( wrap  )

import Autolib.ToDoc


type Env = FiniteMap Identifier Type

env0 :: Env
env0 = listToFM 
    [ ( read "map", read "forall a b . (a -> b) -> [a] -> [b]" )
    , ( read "append", read "forall a . [a] -> [a] -> [a]" )
    , ( read "reverse", read "forall a. [a] -> [a]" )
    , ( read "nil", read "forall a. [a]" )
    , ( read "c", read "forall a b c . (a -> b) -> (b -> c) -> (a -> c)" )
    ]

look :: Env -> Identifier -> Reporter Type
look env at = do
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
    inform $ text "find most general type for" <+> toDoc x
    t0 <- nested 4 $ reconstruct0 env x
    inform $ vcat 
	   [ text "most general type for" <+> toDoc x
	   , nest 4 $ text "is" <+> toDoc ( wrap t0 )
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

    inform $ text "soll ein Funktionstyp sein"
    let [ src, tgt ] = unused 2 $ vars $ unArrow tfun
    let ar = Node ( mkunary "->" ) [ Var src, Var tgt ]

    sigma <- case mgu ar $ unArrow tfun of
        Just sigma -> do
	   inform $ text "OK mit Substitution" <+> toDoc sigma
	   return sigma
	Nothing -> reject $ text "Falsch"
    let src1 = apply sigma $ Var src
	tgt1 = apply sigma $ Var tgt

    -- TODO: typvariablen in arg wegbenennen (konfliktfrei mit fun)
    targ <- reconstructor env arg
    inform $ text "soll passen zum Argumenttyp" <+> toDoc src1 
    rho <- case mgu ( unArrow targ ) src1 of
        Just rho -> do
	    inform $ text "OK mit Substitution" <+> toDoc rho
	    return rho
	Nothing -> reject $ text "Falsch"

    return $ Arrow $ apply_partial rho tgt1



