module NFA.Test where

--  $Id$

import NFA.Property

import Autolib.NFA
import Autolib.NFA.Trim
import Autolib.NFA.Check
import Autolib.NFA.Det
import Autolib.NFA.Normalize
import Autolib.NFA.Minimize

import Autolib.Reporter
import Autolib.Reporter.Set
import Autolib.ToDoc
import Autolib.Size

test :: NFAC c s
     => Property c 
     -> NFA c s
     -> Reporter ()

test (Min_Size s) aut = do
    assert ( size aut >= s ) 
	   $ text "Zustandszahl ist wenigstens" <+> toDoc s <+> text "?"

test (Max_Size s) aut = do
    assert ( size aut <= s ) 
	   $ text "Zustandszahl ist hˆchstens" <+> toDoc s <+> text "?"

test (Alphabet m) aut = do
    subeq ( text "Alphabet des Automaten", alphabet aut )
          ( toDoc m, m )

test (Deterministic) aut = do
    deterministisch aut

test (Reduced) aut = do
    inform $ text "Der Automat soll reduziert sein."
    let unreach = states aut `minusSet` states ( reachable aut )
    when ( not $ isEmptySet unreach ) $ reject 
	 $ vcat [ text "Diese Zust‰nde sind nicht erreichbar:"
		, nest 4 $ toDoc unreach
		]
    let unprod = states aut `minusSet` states ( productive aut )
    when ( not $ isEmptySet unprod ) $ reject 
	 $ vcat [ text "Diese Zust‰nde sind nicht produktiv:"
		, nest 4 $ toDoc unprod
		]
    inform $ text "OK"

test (Minimal) aut = do
    test Complete aut
    test Deterministic aut
    inform $ text "Der Automat soll minimal sein."
    let d = minimize0 $ normalize $ det aut
    when ( size d < size aut ) $ reject
	 $ vcat [ text "Es gibt einen ‰quivalenten deterministischen Automaten"
		, text "mit nur" <+> toDoc (size d) <+> text "Zust‰nden."
		]
    inform $ text "OK"

test Complete aut = do
    inform $ text "Der Automat soll vollst‰ndig sein."
    when ( isEmptySet $ starts aut ) $ reject
	 $ text "Es muﬂ wenigstens einen Startzustand geben."
    let miss = do
            p <- lstates aut
	    c <- setToList $ alphabet aut
	    let qs = lookupset (trans aut) (p, c)
	    guard $ isEmptySet qs
	    return ( p, c )
    when ( not $ null miss ) $ reject $ vcat 
	 [ text "F¸r wenigstens diese (p, c) gibt es keine Transition (p, c, q):"
	 , nest 4 $ toDoc $ take 4 miss
	 ]
		 
test prop aut = do
    reject $ fsep [ text "test f¸r", toDoc prop
		  , text "noch nicht implementiert"
		  ]
