{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Sortier.Netz.Check where

import Sortier.Netz.Common

import Sortier.Netz.Type
import Sortier.Netz.Rechnung
import Sortier.Netz.Example
import Sortier.Netz.Bild
import Autolib.Util.Bild

import qualified Autolib.Util.Wort ( alle )
import Data.List ( tails)
import Data.Typeable

import Autolib.Reporter
import Autolib.ToDoc
import qualified Challenger as C
import Inter.Types
import Autolib.Size


check :: Int -> Netz -> Reporter ()
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
    mapM_ verify $ testing soll
    inform $ text "Das Netz hat alle mÃ¶glichen Eingaben korrekt geordnet."
    return ()


data Sortier = Sortier deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Verify Sortier Int where

    verify p i = 
	assert ( i > 0)
	       $ text "die Breite soll eine positive Zahl sein"

instance C.Partial Sortier Int Netz where

    describe p i = vcat 
	  [ text "Finden Sie ein Sortiernetz für"
	         <+> toDoc i <+> text "Eingänge"
	  , text "mit weniger als" <+> toDoc ( size $ bubble i )
		 <+> text "Komparatoren."
	  ]        

    initial p i   = bubble i
    partial p i b =  do
        inform $ text "Ihr Netz ist:" <+> toDoc ( toBild b )
        let ist  = high b - low b + 1
        when ( i /= ist ) $ reject $ vcat
	     [ text "Das Netz soll Breite" <+> toDoc i <+> text "haben"
	     , text "es hat aber" <+> toDoc ist
	     ]
    total   p i b =  do
	check i b
        when ( size b >= size ( bubble i ) ) $ reject $ vcat
	     [ text "Das sind zuviele Komparatoren."
	     ]

make :: Make
make = direct Sortier (5 :: Int)
