{-# OPTIONS -fallow-incoherent-instances #-}

module LCS.Instance where

--  $Id$

import LCS.Code
import LCS.Data
import LCS.Quiz
import LCS.Config
import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Data.Typeable

import Autolib.ToDoc
import Autolib.Size
import Autolib.Xml
import Autolib.Reporter

data LCS = LCS deriving ( Show, Typeable )

instance ( InstanceC a ) 
    => Partial LCS ( Instance a ) [ a ] where

    describe LCS i =
        vcat [ fsep [ text "Bestimmen Sie eine"  
		    , text $ if sharp i then "l�ngste" else "lange" 
		    , text "gemeinsame Teilfolge"
		    ]
	     , text "von" <+> toDoc ( left i )
	     , text "und" <+> toDoc ( right i )
	     ]
	
    initial LCS i =
        let 
	    (lo, _) = halves $ left  i
	    (_, hi) = halves $ right i
	in  merge lo hi

    partial LCS i zs = do
        let xs = left i ; ys = right i
        assert ( zs `is_embedded_in` xs )
	       ( fsep [ text "ist", toDoc zs, text "Teilfolge von", toDoc xs ])
        assert ( zs `is_embedded_in` ys )
	       ( fsep [ text "ist", toDoc zs, text "Teilfolge von", toDoc ys ])
	
    total LCS i zs = do
	let zs' = lcs (left i) (right i)
	if sharp i
	   then case compare (length zs) (length zs') of
	     LT -> reject $ text "Das ist keine l�ngste gemeinsame Teilfolge."
	     EQ -> inform $ text "Das ist eine l�ngste gemeinsame Teilfolge."
	     GT -> inform $ vcat 
			  [ text "Das kann eigentlich nicht sein."
			  , text "Ihre Einsendung"
			  , nest 4 $ toDoc zs
			  , text "ist l�nger als meine L�sung"
			  , nest 4 $ toDoc zs'
			  ]
	   else if length zs < ( length zs' `div` 2 )
		then reject 
		   $ text "Ihre Folge ist k�rzer als die H�lfte meiner L�sung."
		else return ()

merge :: [a] -> [a] -> [a]
merge [] ys = ys
merge (x : xs) ys = x : merge ys xs

halves :: [a] -> ([a],[a])
halves xs = splitAt (length xs `div` 2) xs
	    
instance InstanceC a => Measure LCS ( Instance a ) [a] where
    measure LCS i zs = fromIntegral $ length zs

make_fixed :: Make
make_fixed = direct LCS LCS.Data.example

instance InstanceC a => Generator LCS (Config a) ( [a], Instance a ) where
    generator _ conf key = roll conf
instance InstanceC a => Project LCS  ( [a], Instance a ) ( Instance a ) where
    project _ ( l, i ) = i

make_quiz :: Make
make_quiz = quiz LCS LCS.Config.example

