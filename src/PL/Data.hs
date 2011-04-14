{-# LANGUAGE DeriveDataTypeable #-}

module PL.Data where


import Autolib.TES.Identifier
import Autolib.Size

import Data.Typeable

data Term = Variable Identifier
	  | Apply Identifier [ Term ]
     deriving ( Eq, Ord, Typeable )

instance Size Term where
     size t = case t of
         Variable _ -> 1
         Apply _ args -> succ $ sum $ map size args

data Quantor = Forall | Exists 
	     | Count Compare Integer
     deriving ( Eq, Ord, Typeable )

data Compare = Less_Equal |  Less -- HACK: need this order for parsing
	      | Equal | Not_Equal | Greater_Equal | Greater
     deriving ( Eq, Ord, Typeable, Enum, Bounded )

data Formel = Quantified Quantor Identifier Formel
	    | Predicate Identifier [ Term ]
	    | Operation PL.Data.Operator [ Formel ]
	    | Equals Term Term 
     deriving ( Eq, Ord, Typeable )

data Operator = Not | And | Or | Iff | Implies
     deriving ( Eq, Ord, Typeable )


        

