module Grammatik.Reduziert 

-- -- $Id$

( reduziert
, reduktion
)

where

-- verallgemeinert from Grammatik.CF.*
-- (benutze sichere Approximationen)

import Grammatik.Type
import Grammatik.Produktiv
import Grammatik.Erreichbar

import Autolib.Reporter
import Autolib.ToDoc

import Control.Monad (guard)
import Autolib.Set

-- | behalte nur die variablen aus qs
-- lˆsche alle regeln, die andere variablen benutzen
einschraenkung :: Set Char -> Grammatik -> Grammatik
einschraenkung qs g =
  g { variablen = sfilter (`elementOf` qs) ( variablen g )
          -- `union` ( unitSet $ startsymbol g ) -- das muﬂ bleiben?
    , regeln = mkSet $ do 
	          rule @ ( lhs, rhs ) <- rules g
		  guard $ and $ do p <- lhs 
                                   return $ ( p `elementOf` terminale g )
                                            || ( p `elementOf` qs )
		  guard $ and $ do x <- rhs
				   guard $ x `elementOf` nichtterminale g
				   return $ x `elementOf` qs
		  return rule
    }

reduktion :: Grammatik -> Grammatik
reduktion g = let
    p = produktiv g
    pg = einschraenkung p g
    e = erreichbar pg
    eg = einschraenkung e pg
  in   eg

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



