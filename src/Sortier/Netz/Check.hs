module Sortier.Netz.Check where

-- $Id$

import Sortier.Netz.Type
import Sortier.Netz.Rechnung
import Sortier.Netz.Example

import qualified Util.Wort ( alle )
import List ( tails)

import Reporter
import ToDoc
import qualified Challenger as C
import Inter.Types
import Size

check :: Int -> Netz -> Reporter ()
check soll n = do
    let verify xs = do
	  let xss = rechnung n xs
	  when ( not $ is_increasing $ last xss ) 
	       $ reject $ vcat
	       [ text "Diese Eingabe wird nicht korrekt geordnet:"
	       , nest 4 $ toDoc xs
	       , text "Die Rechung des Netzes ist:"
	       , nest 4 $ toDoc xss
	       ]
    mapM_ verify $ Util.Wort.alle [ 0, 1 ] soll
    inform $ text "Das Netz hat alle möglichen Eingaben korrekt geordnet."
    return ()

is_increasing :: Ord a => [a] -> Bool
is_increasing xs = and $ do
    x : y : rest <- tails xs
    return $ x <= y

data Sortier = Sortier deriving ( Eq, Ord, Show, Read )

instance C.Partial Sortier Int Netz where
    initial p i   = bubble i
    partial p i b =  do
        let ist  = high b - low b + 1
        when ( i /= ist ) $ reject $ vcat
	     [ text "Das Netz soll Breite" <+> toDoc i <+> text "haben"
	     , text "es hat aber" <+> toDoc ist
	     ]
        when ( size b >= size ( bubble i ) ) $ reject $ vcat
	     [ text "Das Netz soll weniger Komparatoren haben"
	     , text "als das einfache Bubblesort-Netzwerk:"
	     , nest 4 $ toDoc $ bubble i
	     ]
        
    total   p i b = check i b

synthese :: String -- aufgabe (major)
         -> String -- aufgabe (minor)
         -> Int
         -> Var  Sortier Int Netz
synthese auf ver i =
    Var { problem = Sortier
        , aufgabe = auf
        , version = ver
        , key = \ matrikel -> do
              return ""
        , gen = \ key -> return $ do
              inform $ vcat 
	             [ text "Finden Sie ein Sortiernetz für"
	                  <+> toDoc i <+> text "Eingänge"
		     , text "mit weniger als" <+> toDoc ( size $ bubble i )
		          <+> text "Komparatoren."
		     ]
	      return i
        }

generates :: [ IO Variant ]
generates = do 
    w <- [ 4 .. 9 ]
    return $ return $ Variant $ synthese "Netz" ( show w ) w

