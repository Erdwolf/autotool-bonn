
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
import qualified  Grammatik.Checker as C
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

typ0 :: C.Type
typ0 = C.make "Typ0" ( text "Die Grammatik soll vom Typ-0 sein." ) $ \ g -> do

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

monoton :: C.Type 
monoton = C.make "Monoton" ( text "Die Grammatik soll monoton sein." ) $ \ g -> do

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

kontextsensitiv :: C.Type
kontextsensitiv = C.make "CS" ( text "Die Grammatik soll kontextsensitiv sein." ) $ \ g -> do
    let zs = filterSet (null . kontexte g ) $ regeln g
    verboten ( not $ isEmptySet zs ) 
	     "haben nicht die Form  c V d -> c w d:"
	     zs

---------------------------------------------------------------------------

kontextfrei :: C.Type
kontextfrei = 
  C.make "CF" ( text "Die Grammatik soll kontextfrei sein." ) $ \ g -> do
    let lang = filterSet ( (> 1) . length . fst ) $ regeln g
    verboten ( not $ isEmptySet lang ) 
	     "haben nicht die Form V -> w:"
	     lang

-----------------------------------------------------------------------------

linear :: C.Type
linear = C.make "Lin" ( text "Die Grammatik soll linear sein." ) $ \ g -> do
    let schlecht = filterSet 
	   ( (>1) . length . filter (`elementOf` nichtterminale g) . snd ) 
	   $ regeln g
    verboten ( not $ isEmptySet schlecht ) 
	     "enthalten rechts mehr als ein Nichtterminal:"
	     schlecht

------------------------------------------------------------------------------

rechtslinear :: C.Type
rechtslinear = C.make "RightLin" ( text "Die Grammatik soll rechtslinear sein." ) $ \ g -> do
    let schlecht 
	  = filterSet ( not . ( `elementOf` nichtterminale g ) . last . snd ) 
	  $ filterSet ( (> 0) . length . filter ( `elementOf` nichtterminale g ) . snd ) 
	  $ regeln g
    verboten ( not $ isEmptySet schlecht ) 
	     "sind nicht von der Form M -> A^* M"
	     schlecht

------------------------------------------------------------------------------

linkslinear :: C.Type
linkslinear = C.make "LeftLin" ( text "Die Grammatik soll linkslinear sein." ) $ \ g -> do
    let schlecht 
	  = filterSet ( not . ( `elementOf` nichtterminale g ) . head . snd ) 
	  $ filterSet ( (> 0) . length . filter ( `elementOf` nichtterminale g ) . snd ) 
	  $ regeln g
    verboten ( not $ isEmptySet schlecht )
	     "sind nicht von der Form M -> M A^*"
	     schlecht

---------------------------------------------------------------------------

epsfrei :: C.Type
epsfrei = C.make "EpsFree" ( text "Die Grammatik soll Epsilon-frei sein." ) $ \ g -> do
    let schlecht = filterSet ( \ (lhs, rhs) -> null rhs ) $ regeln g
    verboten ( not $ isEmptySet schlecht )  
	 "sind verboten:"
	 schlecht

kettenfrei :: C.Type
kettenfrei = C.make "ChFree" ( text "Die Grammatik soll kettenfrei sein." ) $ \ g -> do
    let schlecht = filterSet ( \ (lhs, rhs) -> 
	 length rhs == 1 && head rhs `elementOf` nichtterminale g ) $ regeln g
    verboten ( not $ isEmptySet schlecht ) 
	 "sind verboten:"
	 schlecht

chomsky :: C.Type
chomsky = C.make "CNF" ( text "Die Grammatik soll in Chomsky-Normalform sein." ) $ \ g -> do
    let ok rhs = ( length rhs == 1 && head rhs `elementOf` terminale g )
              || ( length rhs == 2 && and [ x `elementOf` nichtterminale g 
					  | x <- rhs ] )
	schlecht = filterSet ( \ ( lhs, rhs) -> not (ok rhs) ) $ regeln g
    verboten ( not $  isEmptySet schlecht )
	 "sind nicht von der Form N -> T oder N -> N N"
	 schlecht

greibach :: C.Type
greibach = C.make "Greibach" ( text "Die Grammatik soll in Greibach-Normalform sein." ) $ \ g -> do
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

combine :: String -> [ C.Type ] -> C.Type
combine msg cs = C.make msg 
    ( text "Die Grammatik soll vom"  <+> text msg <+> text "sein." ) $ \ g -> do
      f <- wrap $ nested 4 $ sequence_ $ do c <- cs ; return $ C.run c g 
      let answer f = fsep 
	    [ text "Das ist", text (if f then "" else "k" ) <>  text "eine" 
	    , text msg <> text "-Grammatik."
	    ]
      when ( isNothing f ) $ reject 
	     $ answer False 
      inform $ answer True
      newline

----------------------------------------------------------------------------

typ1 = combine "Typ1"
       $ [ typ0, monoton ] 
       
typ2 = combine "Typ2"
       $ [ typ0, kontextfrei ]

typ3 = combine "Typ3"
       $ [ typ0, kontextfrei, linear, rechtslinear ]

immergut :: C.Type
immergut = C.make "" empty $ \ g -> return ()

