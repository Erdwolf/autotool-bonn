module Grammatik.CF.Reduziert 

-- $Id$

( reduziert
, reduktion
)

where

import Grammatik.Type
import Grammatik.CF.Produktiv
import Grammatik.CF.Erreichbar

import Reporter
import ToDoc

import Control.Monad (guard)
import Sets

einschraenkung :: Set Char -> Grammatik -> Grammatik
einschraenkung qs g =
  g { nichtterminale = sfilter (`elementOf` qs) $ nichtterminale g
    , regeln = mkSet $ do 
	          rule @ ( [lhs], rhs ) <- rules g
		  guard $ lhs `elementOf` qs
		  guard $ and $ do x <- rhs
				   guard $ x `elementOf` nichtterminale g
				   return $ x `elementOf` qs
		  return rule
    }

reduktion :: Grammatik -> Grammatik
reduktion g = let
    e = erreichbar g
    eg = einschraenkung e g
    p = produktiv eg
    pg = einschraenkung p eg
  in   pg

reduziert :: Grammatik -> Reporter ()
reduziert g = do
    let
        a = nichtterminale g
        e = erreichbar g
        ne = minusSet a e
        eg = einschraenkung e g
        p = produktiv eg
        np = minusSet e p
        pg = einschraenkung p eg
    when ( not $ isEmptySet ne ) $ reject $ vcat
	 [ text $ "Diese Variablen sind nicht erreichbar:" 
	 , nest 4 $ toDoc ne
	 ]
    when ( not $ isEmptySet np ) $ reject $ vcat
	 [ text $ "Diese Variablen sind nicht produktiv:"
	 , nest 4 $ toDoc np
	 ]
    inform $ text "Diese Grammatik ist reduziert."



