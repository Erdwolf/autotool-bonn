-- | Korrekturfunktion für Faktorisierung

-- joe@informatik.uni-leipzig.de
-- benutzt code für challenger/PCProblem
-- von Markus Kreuz  mai99byv@studserv.uni-leipzig.de

module Faktor.Faktor (
      make_fixed 
     , make_quiz
    ) where

--  $Id$

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Ana

import Inter.Types
import Inter.Quiz

import Faktor.Type
import Faktor.Quiz
import qualified Faktor.Param

-------------------------------------------------------------------------------

instance OrderScore Faktor where
    scoringOrder _ = None

instance Partial Faktor (Int,Integer) [Integer] where

    describe Faktor (n,x) = vcat
	   [ text $ foldl1 (++) [ "Gesucht sind " 
                                , show n , " Zahlen x_1 .. x_" 
                                , show n , " mit x_i > 1" ]
	   , text ( foldl1 (++) [ "und product " , l , " =" ] ) <+> toDoc x
	   ]
           where l = "[ x_1 , ... , x_" ++ show n ++ " ]"

    initial Faktor (_,x) = 
        let -- do something silly to get candidates of useful size
            b = 3
	    xs = based b x
	    (ys, zs) = splitAt (length xs `div` 2) xs
	in  [unbased b ys, unbased b zs]

    partial Faktor (n,x) fs = do
           assert (length fs == n) $ text $ foldl1 (++) 
                      [ "Es sollen genau " , show n , " Zahlen sein." ]
	   assert (all (>1) fs ) $ text "Alle Zahlen sollen > 1 sein."

    total Faktor (_,x) fs = do
        let p = product fs

        inform $ fsep [ text "Das Produkt der Zahlen"
		      , toDoc fs, text "ist" , toDoc p, text "."
		      ]
        inform $ fsep [ text "Gilt ", toDoc p 
		      , text "=" , toDoc x 
		      , text "?" 
		      ]

        when (x /= p) $ reject $ text "Nein."

        inform $ text "Ja."

-------------------------------------------------------------------------------

make_quiz :: Make
make_quiz = quiz Faktor Faktor.Param.p

make_fixed :: Make
make_fixed = direct Faktor (3::Int,1001::Integer)
