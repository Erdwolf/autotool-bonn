module LCS.Instance where

--  $Id$

import LCS.Code
import Challenger.Partial
import Inter.Types

import ToDoc
import Size
import Reporter

data LCS = LCS deriving ( Eq, Ord, Show, Read )

data Instance a =
     Instance { left :: [a]
	      , right :: [a]
	      , sharp :: Bool
	      }
     deriving ( Read, Show )

instance ( Eq a, ToDoc [a], Size [a] ) => Partial LCS ( Instance a ) [a] where

    describe LCS i =
        vcat [ fsep [ text "Bestimmen Sie eine"  
		    , text $ if sharp i then "l�ngste" else "lange" 
		    , text "gemeinsame Teilfolge"
		    ]
	     , text "von" <+> toDoc ( left i )
	     , text "und" <+> toDoc ( right i )
	     ]
	
    initial LCS i =
        let merge [] ys = ys
	    merge (x : xs) ys = x : merge ys xs
	    halves xs = splitAt (length xs `div` 2) xs
	    (lo, _) = halves $ left i
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

    measure LCS i zs = length zs

fixed :: String 
      -> Bool
      -> ( [a],[a] ) 
      -> Var LCS (Instance a) [a]
fixed name sh ( xs, ys ) = Var
	  { problem = LCS
	  , aufgabe = "LCS"
	  , version = name
	  , key = \ matrikel -> return matrikel
	  , gen = \ key -> do
		return $ return 
		       $ Instance { left = xs, right = ys, sharp = sh }
	  }
