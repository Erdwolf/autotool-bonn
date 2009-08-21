{-# LANGUAGE TemplateHaskell #-}

module Grammatik.CF.Chomsky.Type where

--  $Id$

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Size

type Rules a   = [ ( a, Either Char (a, a) ) ]

data Chomsky a = Chomsky 
	       { start :: a
	       , rules :: Rules a
	       , eps :: Bool -- ^ die ursprüngliche G. erzeugte Eps ?
	       }
     deriving (Eq, Ord)

instance Size (Chomsky a) where 
    size = length . rules

instance Functor Chomsky where
    fmap f ch = 
	 Chomsky { start = f $ start ch
		 , rules = do
		      ( lhs, rhs ) <- rules ch 
		      return ( f lhs 
			     , case rhs of 
				    Left c -> Left c
				    Right (x,y) -> Right (f x, f y) 
		             )
		 , eps = eps ch
		 }

$(derives [makeReader, makeToDoc] [''Chomsky])

-- local variables:
-- mode: haskell
-- end:


