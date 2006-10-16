module Rules

( Rule (..)
, rules
, modusponens
)

where

import FiniteMap
import Set

import Ids
import Syntax

import Sub
import Read

import Ops
import Axioms
import Env


type Rule = ([Exp], Exp) -- Prämissen, Konklusion

mkrules' :: Exp -> [ Rule ]
-- nimm Axiom (X1 -> (X2 -> .. (Xn -> Y) ..) )
-- mache Regeln ( [ X1, .. Xk-1],  (Xk -> ..) )
mkrules' ax = 
  ([], ax) :
  case ax of 
       App fun [ left, right ] | fun == implies ->
	   [ ( left : prems, conc ) 
	   | ( prems, conc ) <- mkrules' right
	   ]
       _ -> []

mkrules = mkrules'

modusponens = 
    let [ a, ab, b ] = parsed . unlines $ [ "A", "A -> B", "B" ]
    in	( [a, ab], b)

rules = modusponens : 
  do ax <- axioms
     mkrules ax




