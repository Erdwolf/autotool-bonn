{-# OPTIONS -fglasgow-exts  #-}

module PL.Data where


import Autolib.TES.Identifier


import Data.Typeable

data Term = Variable Identifier
	  | Apply Identifier [ Term ]
     deriving ( Eq, Ord, Typeable )

data Quantor = Forall | Exists
     deriving ( Eq, Ord, Typeable )

data Formel = Quantified Quantor Identifier Formel
	    | Predicate Identifier [ Term ]
	    | Operation PL.Data.Operator [ Formel ]
     deriving ( Eq, Ord, Typeable )

data Operator = Not | And | Or | Iff | Implies
     deriving ( Eq, Ord, Typeable )


        

