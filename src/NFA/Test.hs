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

test Sane aut = do
    inform $ text "Der Automat soll konsistent definiert sein."
    subeq ( text "Startzustände" , starts aut )
	  ( text "Zustandsmenge" , states aut )
    subeq ( text "akzeptierende Zustände" , finals aut )
	  ( text "Zustandsmenge" , states aut )
    verify "Zustände" 
          ( \ (p,c,q) -> p `elementOf` states aut && q `elementOf` states aut )
	  ( trans aut )
    verify "Buchstaben"
          ( \ (p,c,q) -> c `elementOf` alphabet aut )
	  ( trans aut )

test (Min_Size s) aut = do
    assert ( size aut >= s ) 
	   $ text "Zustandszahl ist wenigstens" <+> toDoc s <+> text "?"

test (Max_Size s) aut = do
    assert ( size aut <= s ) 
	   $ text "Zustandszahl ist höchstens" <+> toDoc s <+> text "?"

test (Alphabet m) aut = do
    subeq ( text "Alphabet des Automaten", alphabet aut )
          ( toDoc m, m )

test (Deterministic) aut = do
    deterministisch aut

test (Non_Deterministic) aut = do
    inform $ text "Der Automat soll nicht deterministisch sein."
    let nondets = do
	  ( (p, c), qs ) <- fmToList $ trans aut
	  guard $ 1 < cardinality qs
	  return ((p, c), qs)
    when ( null nondets ) $ reject $ vcat
	 [ text "es gibt keinen Zustand p und Buchstaben c"
	 , text "mit mehr als einem Nachfolgezustand."
	 ]
    inform $ text "OK"

test (Reduced) aut = do
    inform $ text "Der Automat soll reduziert sein."
    let unreach = states aut `minusSet` states ( reachable aut )
    when ( not $ isEmptySet unreach ) $ reject 
	 $ vcat [ text "Diese Zustände sind nicht erreichbar:"
		, nest 4 $ toDoc unreach
		]
    let unprod = states aut `minusSet` states ( productive aut )
    when ( not $ isEmptySet unprod ) $ reject 
	 $ vcat [ text "Diese Zustände sind nicht produktiv:"
		, nest 4 $ toDoc unprod
		]
    inform $ text "OK"

test (Minimal) aut = do
    test Complete aut
    test Deterministic aut
    inform $ text "Der Automat soll minimal sein."
    let d = minimize0 $ normalize $ det aut
    when ( size d < size aut ) $ reject
	 $ vcat [ text "Es gibt einen äquivalenten deterministischen Automaten"
		, text "mit nur" <+> toDoc (size d) <+> text "Zuständen."
		]
    inform $ text "OK"

test Complete aut = do
    inform $ text "Der Automat soll vollständig sein."
    when ( isEmptySet $ starts aut ) $ reject
	 $ text "Es muß wenigstens einen Startzustand geben."
    let miss = do
            p <- lstates aut
	    c <- setToList $ alphabet aut
	    let qs = lookupset (trans aut) (p, c)
	    guard $ isEmptySet qs
	    return ( p, c )
    when ( not $ null miss ) $ reject $ vcat 
	 [ text "Für wenigstens diese (p, c) gibt es keine Transition (p, c, q):"
	 , nest 4 $ toDoc $ take 4 miss
	 ]
    inform $ text "OK"
		 
test prop aut = do
    reject $ fsep [ text "test für", toDoc prop
		  , text "noch nicht implementiert"
		  ]

verify msg ok tr = do
    let wrong = filter ( not . ok ) $ unCollect tr
    when ( not $ null wrong ) $ reject $ vcat
	 [ text "diese Transitionen benutzen nicht deklarierte" <+> text msg
	 , nest 4 $ toDoc wrong
	 ]

