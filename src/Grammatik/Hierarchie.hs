
module Grammatik.Hierarchie

-- $Id$

( typ0
, typ1
, typ2
, typ3
, immergut

, monoton
, kontextsensitiv
, kontextfrei
, linear
, rechtslinear, linkslinear

, epsfrei
, kettenfrei
, chomsky
, greibach
)

where

import Grammatik.Type
import Wort

import Reporter
import ToDoc

import Util.Splits
import Monad (guard)
import List (partition)
import Set
import FilterSet
import Maybe


---------------------------------------------------------------------------



typ0 :: Grammatik -> Reporter ()
typ0 g = do
    inform $ text "Ist das eine Typ-0-Grammatik?"

    let us = filterSet ( \ (l, r) -> 
	    any ( not . ( `elementOf` ( terminale g `union` nichtterminale g ) ) ) 
		     (l ++ r)  ) 
	   $ regeln g
    when ( not $ isEmptySet us ) $ reject $ fsep
	 [ text "Diese Regeln enthalten ein Zeichen,"
	 , text "das weder Terminal noch Nichtterminal ist:"
	 , toDoc us
	 ]

    let cs = intersect (terminale g) ( nichtterminale g )
    when ( not $ isEmptySet cs ) $ reject $ fsep 
	 [ text "Terminal- und Nichtterminalmenge sind nicht disjunkt."
	 , text "Der Durchschnitt ist:"
	 , toDoc cs
	 ]

    let rs = filterSet ( \ (l, r) -> all ( `elementOf` (terminale g)) l ) 
	   $ regeln g 
    verboten ( not $ isEmptySet rs ) 
	     "enthalten links kein Nichtterminal"
	     rs
    
---------------------------------------------------------------------------

monoton :: Grammatik -> Reporter ()
monoton g = do 
    inform $ text "Ist das eine monotone Grammatik?"    

    let (eps, noeps) = partition ( null . snd ) $ rules g
	weps = mkSet $ filter ((/= [startsymbol g]) . fst) eps

    when ( not $ isEmptySet weps ) $ reject $ fsep
	 [ text "Das leere Wort darf nur aus dem Startsymbol erzeugt werden."
	 , text "Diese Regeln sind deswegen verboten:"
	 , toDoc weps
	 ]

    let feps = mkSet $ do -- falls leeres wort erzeugt wird
	      guard $ not $ null $ eps
	      (l,r) <- rules g
	      guard $ startsymbol g `elem` r
	      return (l, r)
	
    when ( not $ isEmptySet feps ) $ reject $ fsep 
	 [ text "Wenn aus dem Startsymbol ein leeres Wort abgeleitet wird,"
	 , text "dann darf das Startsymbol in keiner rechten Regelseite vorkommen."
	 , text "Diese Regeln sind deswegen verboten:"
	 , toDoc feps
	 ]
    
    let	kurz = filter ( \ (l, r) -> length l > length r ) $ noeps
    when ( not $ null kurz ) $ reject $ fsep
	 [ text "Die rechte Regelseite darf nicht kürzer als die linke sein."
	 , text "Diese Regeln sind deswegen verboten:"
	 , toDoc kurz
	 ]

    inform $ text "Ja."

---------------------------------------------------------------------------

kontexte :: Grammatik 
	 -> (String, String) -> [ ((String, String), (Char, String)) ]
kontexte g (lhs, rhs) = do
    (pre, x : post) <- splits lhs
    guard $ x `elementOf` nichtterminale g
    let (vorn, rest) = splitAt (length pre) rhs
    guard $ pre == vorn
    let (mitte, hinten) = splitAt (length rest - length post) rest
    guard $ post == hinten
    return $ ((pre, post), (x, mitte))

kontextsensitiv :: Grammatik -> Reporter ()
kontextsensitiv g = do
    inform $ text "Ist das eine kontextsensitive Grammatik?"
    let zs = filterSet (null . kontexte g ) $ regeln g
    verboten ( not $ isEmptySet zs ) 
	     "haben nicht die Form  c V d -> c w d:"
	     zs

---------------------------------------------------------------------------

kontextfrei :: Grammatik -> Reporter ()
kontextfrei g = do
    inform $ text "Ist das eine kontextfreie Grammatik?"
    let lang = filterSet ( (> 1) . length . fst ) $ regeln g
    verboten ( not $ isEmptySet lang ) 
	     "haben nicht die Form V -> w:"
	     lang

-----------------------------------------------------------------------------

linear :: Grammatik -> Reporter ()
linear g = do
    inform $ text "Ist das eine lineare Grammatik?"
    let schlecht = filterSet 
	   ( (>1) . length . filter (`elementOf` nichtterminale g) . snd ) 
	   $ regeln g
    verboten ( not $ isEmptySet schlecht ) 
	     "enthalten rechts mehr als ein Nichtterminal:"
	     schlecht

------------------------------------------------------------------------------

rechtslinear :: Grammatik -> Reporter ()
rechtslinear g = do
    inform $ text "Ist das eine rechtslineare Grammatik?"
    let schlecht 
	  = filterSet ( not . ( `elementOf` nichtterminale g ) . last . snd ) 
	  $ filterSet ( (> 0) . length . filter ( `elementOf` nichtterminale g ) . snd ) 
	  $ regeln g
    verboten ( not $ isEmptySet schlecht ) 
	     "sind nicht von der Form M -> A^* M"
	     schlecht

------------------------------------------------------------------------------

linkslinear :: Grammatik -> Reporter ()
linkslinear g = do
    inform $ text "Ist das eine linkslineare Grammatik?"
    let schlecht 
	  = filterSet ( not . ( `elementOf` nichtterminale g ) . head . snd ) 
	  $ filterSet ( (> 0) . length . filter ( `elementOf` nichtterminale g ) . snd ) 
	  $ regeln g
    verboten ( not $ isEmptySet schlecht )
	     "sind nicht von der Form M -> M A^*"
	     schlecht

---------------------------------------------------------------------------

epsfrei :: Grammatik -> Reporter ()
epsfrei g = do
    inform $ text "Ist das eine Epsilon-freie Grammatik?"
    let schlecht = filterSet ( \ (lhs, rhs) -> null rhs ) $ regeln g
    verboten ( not $ isEmptySet schlecht )  
	 "sind verboten:"
	 schlecht

kettenfrei :: Grammatik -> Reporter ()
kettenfrei g = do
    inform $ text "Ist das eine kettenfreie Grammatik?"
    let schlecht = filterSet ( \ (lhs, rhs) -> 
	 length rhs == 1 && head rhs `elementOf` nichtterminale g ) $ regeln g
    verboten ( not $ isEmptySet schlecht ) 
	 "sind verboten:"
	 schlecht

chomsky :: Grammatik -> Reporter ()
chomsky g = do
    inform $ text "Ist das eine Grammatik in Chomsky-Normalform?"
    let ok rhs = ( length rhs == 1 && head rhs `elementOf` terminale g )
              || ( length rhs == 2 && and [ x `elementOf` nichtterminale g 
					  | x <- rhs ] )
	schlecht = filterSet ( \ ( lhs, rhs) -> not (ok rhs) ) $ regeln g
    verboten ( not $  isEmptySet schlecht )
	 "sind nicht von der Form N -> T oder N -> N N"
	 schlecht

greibach :: Grammatik -> Reporter ()
greibach g = do
    inform $ text "Ist das eine Grammatik in Greibach-Normalform?"
    let ok rhs = ( length rhs > 0 
		   && head rhs `elementOf` terminale g
		   && and [ x `elementOf` nichtterminale g | x <- tail rhs ]
		 )
	schlecht = filterSet ( \ ( lhs, rhs) -> not (ok rhs) ) $ regeln g
    verboten ( not $ isEmptySet schlecht )
	 "sind nicht von der Form N -> T N^*"
	 schlecht

---------------------------------------------------------------------------

verboten :: ToDoc a => Bool -> String -> a -> Reporter ()
verboten f msg d = do
    when f $ reject $ fsep
	 [ text "Diese Regeln", text msg, text ":"
	 , toDoc d
	 ]
    inform $ text "Ja." 

final :: String -> Reporter () -> Reporter ()
final msg r = do
      inform $ text "Ist das eine"  <+> text msg <> text "?"
      f <- wrap r
      when ( isNothing f ) $ reject 
	     $ text "Das ist keine" <+> text msg <> text "."
      inform $ text "Das ist eine"  <+> text msg <> text "."
      newline

----------------------------------------------------------------------------

typ1 g = final "Typ-1-Grammatik"
       $ sequence_ [ typ0 g, monoton g ] 
       
typ2 g = final "Typ-2-Grammatik"
       $ sequence_ [ typ0 g, kontextfrei g ]

typ3 g = final "Typ-3-Grammatik"
       $ sequence_ [ typ0 g, kontextfrei g, linear g, rechtslinear g ]

immergut g = return ()