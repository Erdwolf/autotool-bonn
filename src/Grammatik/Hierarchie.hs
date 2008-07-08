
module Grammatik.Hierarchie

-- -- $Id$

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
import qualified Autolib.Reporter.Checker as C
import Autolib.Wort

import Autolib.Reporter
import Autolib.ToDoc

import Autolib.Util.Splits
import Control.Monad (guard)
import Data.List (partition)
import Autolib.Set
import Data.Maybe


---------------------------------------------------------------------------

typ0 :: C.Type Grammatik
typ0 = C.make "Typ0" ( text "Die Grammatik soll vom Typ 0 sein." ) $ \ g -> do

    let us = sfilter ( \ (l, r) -> 
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

    let rs = sfilter ( \ (l, r) -> all ( `elementOf` (terminale g)) l ) 
	   $ regeln g 
    verboten ( not $ isEmptySet rs ) 
	     "enthalten links kein Nichtterminal"
	     rs
    
---------------------------------------------------------------------------

monoton :: C.Type Grammatik 
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

kontextsensitiv :: C.Type Grammatik
kontextsensitiv = C.make "CS" ( text "Die Grammatik soll kontextsensitiv sein." ) $ \ g -> do
    let zs = sfilter (null . kontexte g ) $ regeln g
    verboten ( not $ isEmptySet zs ) 
	     "haben nicht die Form  c V d -> c w d:"
	     zs

---------------------------------------------------------------------------

kontextfrei :: C.Type Grammatik
kontextfrei = 
  C.make "CF" ( text "Die Grammatik soll kontextfrei sein." ) $ \ g -> do
    let lang = sfilter ( (> 1) . length . fst ) $ regeln g
    verboten ( not $ isEmptySet lang ) 
	     "haben nicht die Form V -> w:"
	     lang

-----------------------------------------------------------------------------

linear :: C.Type Grammatik
linear = C.make "Lin" ( text "Die Grammatik soll linear sein." ) $ \ g -> do
    let schlecht = sfilter 
	   ( (>1) . length . filter (`elementOf` nichtterminale g) . snd ) 
	   $ regeln g
    verboten ( not $ isEmptySet schlecht ) 
	     "enthalten rechts mehr als ein Nichtterminal:"
	     schlecht

------------------------------------------------------------------------------

rechtslinear :: C.Type Grammatik
rechtslinear = C.make "RightLin" ( text "Die Grammatik soll rechtslinear sein." ) $ \ g -> do

    let schlecht = do
          lr @ ( l, r ) <- setToList $ regeln g
          case reverse r of
              x : xs | any (`elementOf` nichtterminale g ) xs ->
                  return lr
              _ -> []

    verboten ( not $ null schlecht ) 
	     "sind nicht von der Form V -> T^* V"
	     schlecht

streng_rechtslinear :: C.Type Grammatik
streng_rechtslinear = C.make "StrictRightLin" 
                      ( text "Die Grammatik soll rechtslinear sein." ) $ \ g -> do

    let schlecht = do
          lr @ ( l, r ) <- setToList $ regeln g
          let ok = case r of
                  [] -> True
                  x : ys -> x `elementOf` terminale g && case ys of
                      [] -> True
                      [y] -> y `elementOf` nichtterminale g
                      _ -> False
          guard $ not ok
          return lr

    verboten ( not $ null schlecht ) 
	     "sind nicht von der Form V -> T V, V -> T, V -> Epsilon"
	     schlecht

------------------------------------------------------------------------------

linkslinear :: C.Type Grammatik
linkslinear = C.make "LeftLin" ( text "Die Grammatik soll linkslinear sein." ) $ \ g -> do

    let schlecht = do
          lr @ ( l, r ) <- setToList $ regeln g
          case r of
              x : xs | any (`elementOf` nichtterminale g ) xs ->
                  return lr
              _ -> []

    verboten ( not $ null schlecht )
	     "sind nicht von der Form V -> V T^*"
	     schlecht

---------------------------------------------------------------------------

epsfrei :: C.Type Grammatik
epsfrei = C.make "EpsFree" ( text "Die Grammatik soll Epsilon-frei sein." ) $ \ g -> do
    let schlecht = sfilter ( \ (lhs, rhs) -> null rhs ) $ regeln g
    verboten ( not $ isEmptySet schlecht )  
	 "sind verboten:"
	 schlecht

kettenfrei :: C.Type Grammatik
kettenfrei = C.make "ChFree" ( text "Die Grammatik soll kettenfrei sein." ) $ \ g -> do
    let schlecht = sfilter ( \ (lhs, rhs) -> 
	 length rhs == 1 && head rhs `elementOf` nichtterminale g ) $ regeln g
    verboten ( not $ isEmptySet schlecht ) 
	 "sind verboten:"
	 schlecht

chomsky :: C.Type Grammatik
chomsky = C.make "CNF" ( text "Die Grammatik soll in Chomsky-Normalform sein." ) $ \ g -> do
    let ok rhs = ( length rhs == 1 && head rhs `elementOf` terminale g )
              || ( length rhs == 2 && and [ x `elementOf` nichtterminale g 
					  | x <- rhs ] )
	schlecht = sfilter ( \ ( lhs, rhs) -> not (ok rhs) ) $ regeln g
    verboten ( not $  isEmptySet schlecht )
	 "sind nicht von der Form N -> T oder N -> N N"
	 schlecht

greibach :: C.Type Grammatik
greibach = C.make "Greibach" ( text "Die Grammatik soll in Greibach-Normalform sein." ) $ \ g -> do
    let ok rhs = ( length rhs > 0 
		   && head rhs `elementOf` terminale g
		   && and [ x `elementOf` nichtterminale g | x <- tail rhs ]
		 )
	schlecht = sfilter ( \ ( lhs, rhs) -> not (ok rhs) ) $ regeln g
    verboten ( not $ isEmptySet schlecht )
	 "sind nicht von der Form N -> T N^*"
	 schlecht

---------------------------------------------------------------------------

verboten :: ToDoc a => Bool -> String -> a -> Reporter ()
verboten f msg d = 
    if f 
    then reject $ fsep
	 [ text "Diese Regeln", text msg, text ":"
	 , toDoc d
	 ]
    else inform $ text "Ja." 

combine :: String -> [ C.Type Grammatik ] -> C.Type Grammatik
combine msg cs = C.make msg 
    ( text "Die Grammatik soll vom"  <+> text msg <+> text "sein." ) $ \ g -> do
      f <- wrap $ nested 4 $ sequence_ $ do c <- cs ; return $ C.run c g 
      let answer f = fsep 
	    [ text "Das ist", text (if f then "" else "k" ) <>  text "eine" 
	    , text msg <> text "-Grammatik."
	    ]
      if ( isNothing f ) 
         then   reject $ answer False 
         else   inform $ answer True

----------------------------------------------------------------------------

typ1 = combine "Typ1"
       $ [ typ0, monoton ] 
       
typ2 = combine "Typ2"
       $ [ typ0, kontextfrei ]

typ3 = combine "Typ3"
       $ [ typ0, kontextfrei, linear, rechtslinear ]

immergut :: C.Type Grammatik
immergut = C.make "G" empty $ \ g -> return ()

