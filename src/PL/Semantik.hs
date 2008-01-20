{-# OPTIONS -fglasgow-exts -fallow-incoherent-instances #-}

module PL.Semantik where

import PL.Type
import PL.Interpretation
import PL.Util

import PL.Data
import PL.Interpretation
import PL.Struktur

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Set
import Autolib.FiniteMap

evaluate_top :: ( ToDoc u, Ord u ) 
	 => Interpretation u -> Formel 
	 -> Reporter Bool
evaluate_top int f = case f of
    Operation And fs -> do
        vs <- sequence $ do
            f <- fs
            return $ do
                inform $ vcat [ text "Teilformel", nest 4 $ toDoc f ]
                v <- nested 4 $ evaluate int f
                inform $ vcat [ text "hat Wert", nest 4 $ toDoc f ]
                return v
        return $ and vs
    _ -> evaluate int f

evaluate :: ( ToDoc u, Ord u ) 
	 => Interpretation u -> Formel 
	 -> Reporter Bool
evaluate int f = case f of
    Operation op fs -> evaluate_operation int op fs
    PL.Data.Predicate p args -> evaluate_predicate int p args
    Quantified q v f -> evaluate_quantified int q v f
    Equals l r -> evaluate_equals int l r

evaluate_equals int l r = do
    [ x , y ] <- mapM ( compute int ) [ l, r ]
    return $ x == y

evaluate_quantified int q v f = do
    vals <- sequence $ do
        x <- setToList $ universum $ struktur int
	let extended_int = int { belegung = addToFM ( belegung int ) v x }
	return $ evaluate extended_int f
    let op = case q of Forall -> and ; Exists -> or ; Count c n -> count c n 
    return $ op vals

count c n vals = 
    let m = fromIntegral $ length $ filter id vals
        op = case c of
	   Less -> (<) ; Less_Equal -> (<=) 
	   Equal -> (==) ; Not_Equal -> (/=)
	   Greater_Equal -> (>=) ; Greater -> (>)
    in  m `op` n


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

