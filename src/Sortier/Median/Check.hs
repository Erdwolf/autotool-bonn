{-# LANGUAGE DeriveDataTypeable #-}
module Sortier.Median.Check where

--   $Id$

import Sortier.Median.Param

import Sortier.Netz.Type
import Sortier.Netz.Check ( umrech, is_increasing )

import Sortier.Netz.Rechnung
import Sortier.Netz.Example
import Sortier.Netz.Bild
import Autolib.Util.Bild

import qualified Autolib.Util.Wort ( alle )
import Data.List ( tails, partition )
import Data.Typeable

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Set
import Autolib.FiniteMap
import qualified Challenger as C
import Inter.Types
import Autolib.Size

import Control.Monad ( guard )

data Median_Netz = Median_Netz deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Median_Netz where
    scoringOrder _ = Increasing

instance C.Verify Median_Netz Param where
    verify p i = do
        assert ( breite i > 0)
	       $ text "die Breite soll eine positive Zahl sein"
        assert ( odd $ breite i )
	       $ text "die Breite soll eine ungerade Zahl sein"

instance C.Partial Median_Netz Param Netz where

    describe p i = vcat 
	  [ text "Finden Sie ein Netzwerk mit"
	      <+> toDoc ( breite i ) <+> text "Eingängen,"
	  , text "mit höchstens" <+> toDoc ( max_comparators i )
	  	 <+> text "Komparatoren,"
	  , text "dessen mittlerer Ausgang der Median ist."
	  ]        

    initial p i   = bubble_median ( breite i ) 

    partial p i b =  do
        inform $ text "Ihr Netz ist:" <+> toDoc ( toBild b )
        let ist  = high b - low b + 1
        when ( breite i /= ist ) $ reject $ vcat
	     [ text "Das Netz soll Breite" <+> toDoc i <+> text "haben"
	     , text "es hat aber" <+> toDoc ist
	     ]
    total   p i b =  do
	check ( breite i ) b
        when ( size b > max_comparators i ) $ reject $ vcat
	     [ text "Das sind zuviele Komparatoren."
	     ]

make :: Make
make = 
    let b = 5
    in  direct Median_Netz 
          $ Param { breite = b
		  , max_comparators = pred $ size $ bubble_median b
		  }

bubble_median :: Int -> Netz
bubble_median top = mkNetz $ do
    let mid = 1 + ( top `div` 2 ) 
    t <- reverse [ mid .. top  ]
    s <- [ 1 .. t - 1 ]
    return ( s, 1 + s )

check :: Int -> Netz -> Reporter ()
check soll n = do
    let verify xs = do
	  let xss = rechnung n xs
	  when ( not $ median_is_in_the_middle $ last xss ) 
	       $ reject $ vcat
	       [ text "Diese Eingabe wird nicht korrekt geordnet"
	       , text "(Der Median ist nicht in der Mitte der Ausgabe):"
	       , nest 4 $ toDoc xs
	       , text "Das Netz berechnet die Ausgabe:"
	       , nest 4 $ toDoc $ last xss
	       , text "Die Rechung des Netzes ist:"
	       , nest 4 $ toDoc $ toBild ( n , xss )
	       ]
    mapM_ verify $ map umrech $ testing soll
    inform $ text "Das Netz hat alle möglichen Eingaben korrekt geordnet."
    return ()

median_is_in_the_middle xs = 
    let ( pre, m : post ) = splitAt ( length xs `div` 2 ) xs
        ( small, high ) = partition ( < m ) $ pre ++ post
    in  length small == length high

-- | alle Folgen mit genau floor(b/2) einsen und ceil(b/2) einsen
testing :: Int -> [ State ]
testing b = 
    let h = b `div` 2
    in  selections b h ++ selections b ( h + 1 )

-- | alle Folgen von 0-1 Folgen mit genau k mal 1
selections n 0 = return $ replicate n 0
selections n e = 
       do xs <- selections (n-1) (e-1) ; return $ 1 : xs 
    ++ do guard $ n > e ; xs <- selections (n-1) e     ; return $ 0 : xs 

