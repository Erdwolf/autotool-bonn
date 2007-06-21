{-# OPTIONS -fglasgow-exts #-}

module Fun.Direkt where

import qualified Fun.Quiz as Q

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


instance Partial Primrec_2D ( Matrix  Fun where
    --  Anfangsbeispiel
    initial p i   = Fun.Examples.plus
    -- Partiell Korrekt 
    partial p i b = do           
          check_builtins RAM.Builtin.every b
          check_arity 2 b
    --  Total Korrekt
    total   p i b = do
          inform $ text "Die Wertetabelle Ihrer Funktion ist:"
	  let (dl,ur) = bounds $ unTafel2 i
	  let t = tabulate2 b ur
	  inform $ nest 4 $ toDoc t
          -- Unterschiede berechnen
          let diffs = do
                  xy <- indices $ unTafel2 i
		  let l = unTafel2 i ! xy
		      r = unTafel2 t ! xy
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
    --  Aufgabe beschreiben
    describe p i = 
            vcat [ text "Konstruieren Sie eine zweistellige primitiv rekursive Funktion"
                 , text "mit dieser Wertetabelle:"
                 , nest 4 $ toDoc i
                 ]


instance Generator Fun_Quiz2 Param ( Fun, Tafel2 ) where
    generator _ par key = nontrivial par 
        
instance Project Fun_Quiz2 ( Fun, Tafel2 ) Tafel2 where
    project _ ( f, t ) = t

make :: Make
make = quiz Fun_Quiz2 Fun.Quiz.Type.example

