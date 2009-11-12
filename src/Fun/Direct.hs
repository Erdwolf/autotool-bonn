{-# OPTIONS -fglasgow-exts #-}

module Fun.Direct where

import qualified Fun.Direct.Config as D

import qualified Fun.Quiz.Type2 as T2
import qualified Fun.Quiz.Type as T

import Fun.Type
import Fun.Table
import Fun.Examples
import Fun.Check
import Fun.Create
import qualified RAM.Builtin


import Inter.Types
import Inter.Quiz
import Challenger.Partial
import Autolib.Informed

import Data.Array
import Autolib.Reporter
import Autolib.ToDoc

import Condition

instance Partial D.Primrec_2D D.Config  Fun where
    --  Aufgabe beschreiben
    describe p i = 
            vcat [ text "Konstruieren Sie eine zweistellige primitiv rekursive Funktion"
                 , text "mit dieser Wertetabelle:"
                 , nest 4 $ toDoc $ D.table i
		 , if null $ D.properties i
		   then empty
		   else text "und diesen Eigenschaften:"
		        </> ( vcat $ map explain $ D.properties i )
                 ]

    --  Anfangsbeispiel
    initial p i   = Fun.Examples.plus
    -- Partiell Korrekt 
    partial p i b = do           
          investigate ( D.properties i ) b
          check_arity 2 b
    --  Total Korrekt
    total   p i b = do
          inform $ text "Die Wertetabelle Ihrer Funktion ist:"
          mytafel <- D.mktafel2 $ D.table i
	  let (dl,ur) = bounds $ unTafel2 mytafel
	  let t :: Tafel2
	      t = tabulate2 b ur
	  inform $ nest 4 $ prettyTafel2 t
          -- Unterschiede berechnen
          let diffs = do
                  xy <- indices $ unTafel2 mytafel
		  let l = unTafel2 ( mytafel ) ! xy
		      r = unTafel2 (       t ) ! xy
                  guard $ l /= r
                  return ( xy, l, r )
          -- Bei Unterschieden -> Differenz ausgeben
          when ( not $ null diffs ) $ do
               inform $ text "Die Tabellen stimmen wenigstens hier nicht überein:"
               reject $ nest 4 $ vcat $ take 3 $  do
	           ( xy, l, r ) <- diffs
		   return $ hsep
			  [ text "Argumente:", toDoc xy
			  , text "vorgebener Wert:", toDoc l
			  , text "Ihr Wert:", toDoc r
			  ]
          -- Sehr schoen: richtig Loesung
          inform $ text "Die Tabellen stimmen überein."

make_fixed :: Make
make_fixed = direct D.Primrec_2D D.example


instance Generator D.Primrec_2D T2.Param ( Fun, D.Config ) where
    generator _ par key = do
        ( f, t ) <- nontrivial 
             $ T.Param { T.expression_size = T2.expression_size par
		       , T.table_size = T2.table_size par
		       }
	return ( f
	       , D.Config { D.table = D.mkmatrix t	
			  , D.properties = T2.properties par 
			  } 
	       )
        
instance Project D.Primrec_2D ( Fun, D.Config ) D.Config where
    project _ ( f, c ) = c

make_quiz :: Make
make_quiz = quiz D.Primrec_2D T2.example

