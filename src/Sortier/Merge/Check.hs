{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Sortier.Merge.Check where

--   $Id$

import Sortier.Merge.Param

import Sortier.Netz.Type
import Sortier.Netz.Check ( umrech, is_increasing )

import Sortier.Netz.Rechnung
import Sortier.Netz.Example
import Sortier.Netz.Bild
import Autolib.Util.Bild

import qualified Autolib.Util.Wort ( alle )
import Data.List ( tails)
import Data.Typeable

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Set
import Autolib.FiniteMap
import qualified Challenger as C
import Inter.Types
import Autolib.Size

data Merge_Netz = Merge_Netz deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Verify Merge_Netz Param where

    verify p i = sequence_ $ do
	b <- breiten i
	return $ assert ( b > 0)
	       $ text "jede Breite soll eine positive Zahl sein"

instance C.Partial Merge_Netz Param Netz where

    describe p i = vcat 
	  [ text "Finden Sie ein Netzwerk zum korrekten Zusammenfügen"
	  , text "von bereits geordneten Folgen der Längen" 
              <+> toDoc ( breiten i )
	  , text "mit höchstens" <+> toDoc ( max_comparators i )
	  	 <+> text "Komparatoren."
	  ]        

    initial p i   = bubble_merge ( breiten i ) 

    partial p i b =  do
        inform $ text "Ihr Netz ist:" <+> toDoc ( toBild b )
        let ist  = high b - low b + 1
        when ( gesamt_breite i /= ist ) $ reject $ vcat
	     [ text "Das Netz soll Breite" <+> toDoc i <+> text "haben"
	     , text "es hat aber" <+> toDoc ist
	     ]
    total   p i b =  do
	check ( breiten i ) b
        when ( size b > max_comparators i ) $ reject $ vcat
	     [ text "Das sind zuviele Komparatoren."
	     ]

gesamt_breite :: Param -> Int
gesamt_breite p = sum $ breiten p

make :: Make
make = 
    let bs = [ 5, 3 ]
    in  direct Merge_Netz 
          $ Param { breiten = bs
		  , max_comparators = pred $ size $ bubble_merge bs
		  }

bubble_merge :: [ Int ] -> Netz
bubble_merge bs = remove bs $ bubble $ sum bs


-- | entferne alle Komparatoren (von links),
-- die bereits geordnete Paare vergleichen
remove :: [ Int ] -> Netz -> Netz
remove bs n = 
    let classify n [] = emptyFM
	classify n ( b : bs ) = addListToFM (classify (n+b) bs) $ do
	    i <- [ n+1 .. n+b ] ; return ( i, length bs )
	remo subs [] = []
	remo subs ((x,y) : xys) = 
	    if    lookupFM subs x /= Nothing
	       && lookupFM subs x == lookupFM subs y 
	    then remo subs xys 
	    else (x,y) : remo ( foldl delFromFM subs [x,y] ) xys	    
    in  mkNetz $ remo ( classify 0 bs ) $ comps n


check :: [ Int ] -> Netz -> Reporter ()
check soll n = do
    let verify xs = do
	  let xss = rechnung n xs
	  when ( not $ is_increasing $ last xss ) 
	       $ reject $ vcat
	       [ text "Diese Eingabe wird nicht korrekt geordnet:"
	       , nest 4 $ toDoc xs
	       , text "Das Netz berechnet die Ausgabe:"
	       , nest 4 $ toDoc $ last xss
	       , text "Die Rechung des Netzes ist:"
	       , nest 4 $ toDoc $ toBild ( n , xss )
	       ]
    mapM_ verify $ map umrech $ testing soll
    inform $ text "Das Netz hat alle möglichen Eingaben korrekt geordnet."
    return ()

-- | alle Folgen von monotonen 0-1-Folgen
testing :: [ Int ] -> [ State ]
testing bs = map concat $ mapM stairs bs

stairs :: Int -> [ State ]
stairs b = do
    c <- [ 0 .. b ]
    return $ replicate c 0 ++ replicate ( b - c ) 1
