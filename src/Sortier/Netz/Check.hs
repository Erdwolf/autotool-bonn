module Sortier.Netz.Check where

--   $Id$

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
    inform $ text "Das Netz hat alle möglichen Eingaben korrekt geordnet."
    return ()

testing :: Int -> [State]
testing soll = do
    w <- Autolib.Util.Wort.alle [ 0, 1 ] soll
    -- es gilt ja der satz: 
    -- alle 0-1-folgen sortiert <=> überhaupt alle folgen sortiert.
    -- also generiere ich 0-1-folgen (weil das weniger sind)
    -- aber um die studenten nicht unnötig aufzuregen,
    -- rechne ich es in folgen aus lauter verschiedenen elementen um
    return $ umrech w    

umrech :: [ Int ] -> [ Int ]
-- das ist eine schöne übungsaufgabe
umrech w = um w 1 (length w) where
    um [] _ _ = []
    um (0 : xs) low high = low  : um xs (succ low) high
    um (1 : xs) low high = high : um xs low (pred high)

is_increasing :: Ord a => [a] -> Bool
is_increasing xs = and $ do
    x : y : rest <- tails xs
    return $ x <= y

data Sortier = Sortier deriving ( Eq, Ord, Show, Read, Typeable )

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


synthese :: String -- aufgabe (major)
         -> String -- aufgabe (minor)
         -> Int
         -> Var  Sortier Int Netz
synthese auf ver i =
    Var { problem = Sortier
        , tag = auf ++ "-" ++ ver
        , key = \ matrikel -> do
              return ""
        , gen = \ key -> return $ do
	      return i
        }

generates :: [ IO Variant ]
generates = do 
    w <- [ 4 .. 9 ]
    return $ return $ Variant $ synthese "Netz" ( show w ) w

make :: Make
make = direct Sortier (5 :: Int)
