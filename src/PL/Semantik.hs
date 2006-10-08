{-# OPTIONS -fglasgow-exts -fallow-incoherent-instances #-}

module PL.Semantik where

import PL.Type
import PL.Interpretation

import PL.Data
import PL.Interpretation
import PL.Struktur

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Set
import Autolib.FiniteMap

evaluate :: ( ToDoc u, Ord u ) 
	 => Interpretation u -> Formel -> Reporter Bool
evaluate int f = case f of
    Operation op fs -> evaluate_operation int op fs
    PL.Data.Predicate p args -> evaluate_predicate int p args
    Quantified q v f -> evaluate_quantified int q v f

evaluate_quantified int q v f = do
    vals <- sequence $ do
        x <- setToList $ universum $ struktur int
	let extended_int = int { belegung = addToFM ( belegung int ) v x }
	return $ evaluate extended_int f
    let op = case q of Forall -> and ; Exists -> or
    return $ op vals

evaluate_predicate int p args = do
    PL.Struktur.Predicate r <- 
        find_or_complain "predicate symbol" ( predicates $ struktur int ) p
    vals <- mapM ( compute int ) args 
    return $ vals `elementOf` r

evaluate_operation int op fs = do
    vals <- mapM ( evaluate int ) fs
    case ( op, vals ) of
        ( Not , [x] ) -> return $ not x
        ( And, [x,y] ) -> return $ x && y
        ( Or, [x,y] ) -> return $ x || y    
        ( Implies, [x,y] ) -> return $ x <= y
        ( Iff, [x,y] ) -> return $ x == y
        _ -> reject $ ( text "wrong number of arguments:" ) $$ vcat
                 [ text "operator" <+> toDoc op
                 , text "arguments" <+> toDoc fs
                 , text "values" <+> toDoc vals
                 ]

compute int ( Variable v ) = do
    x <- find_or_complain "variable" ( belegung int ) v
    return x
compute int ( Apply f args ) = do
    Function f <- find_or_complain "function symbol" ( functions $ struktur int ) f
    vals <- mapM ( compute int ) args
    res <- find_or_complain "function value" f vals
    return res

find_or_complain tag fm this = 
    case lookupFM fm this of
        Just x -> return x
	Nothing -> reject $ fsep 
            [ text tag, toDoc this, text "not bound/defined" ]

