{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}
module Sortier.Programm.Check where

import Sortier.Common.Util
import Sortier.Common.Config

import Sortier.Programm.Type
import Sortier.Programm.Exec
import Sortier.Programm.Example

import qualified Autolib.Util.Wort ( alle )
import Data.List ( tails)
import Data.Typeable

import Autolib.Reporter
import Autolib.ToDoc
import qualified Challenger as C
import Inter.Types
import Autolib.Size
import Autolib.Util.Sort ( sortBy )


check :: Int -> Program -> Reporter ()
check breit p = do
    let verify xs = do
	  let ( mres, com ) = export $ Sortier.Programm.Exec.ute p xs
	  case mres of
	       Nothing -> reject $ vcat [ text "Fehler bei Ausführung" , com ]
	       Just xs -> when ( not $ is_increasing xs ) $ reject $ vcat 
			  [ text "Fehler im Resultat (nicht geordnet)"
			  , com 
			  ]
    mapM_ verify $ testing breit
    inform $ text "Das Programm hat alle möglichen Eingaben korrekt geordnet."
    return ()


data Sortier_Programm = Sortier_Programm 
     deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Sortier_Programm where
    scoringOrder _ = Increasing

instance C.Verify Sortier_Programm Config where

    verify p i = do
	assert ( width i > 0)
	       $ text "die Breite (width) soll eine positive Zahl sein"
	assert ( max_size i > 0)
	       $ text "die Größe (max_size) soll eine positive Zahl sein"
	let bound = 1 + truncate ( logBase 2 $ fromIntegral $ product [ 1 ..  width i ] )
	assert ( max_size i >= bound ) $ vcat 
	    [ text "die Größe (max_size) darf nicht kleiner sein"
	    , text "als die informationstheoretische Schranke (log2 (width!))"
	    ]


instance C.Partial Sortier_Programm Config Program where

    describe p i = vcat 
	  [ text "Finden Sie ein Sortierprogramm für"
	         <+> toDoc ( width i ) <+> text "Eingaben,"
	  , text "das für jede Eingabe höchstens" <+> toDoc ( max_size i )
		 <+> text "Vergleiche ausführt."
	  ]        

    initial p i   = nonsense $ names $ width i 

    partial p i b = do
        let s = size b
	inform $ text "Die größtmögliche Anzahl von Vergleichen ist" <+> toDoc s
	when ( s > max_size i ) $ reject $ text "Das ist zuviel."

    total   p i b =  do
	check ( width i ) b

make :: Make
make = direct Sortier_Programm Sortier.Common.Config.example


