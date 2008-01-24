module Hilbert.Proof where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.FiniteMap

import Expression.Op
import Boolean.Op

data Proof 
     = Modus_Ponens { left :: Proof
		    , right :: Proof 
		    }
     | Global_Substitution { fm :: FiniteMap Identifier ( Exp Bool ) 
		    , to :: Proof
		    }
     | Local_Substitution { fm :: FiniteMap Identifier ( Exp Bool ) 
		    , to :: Proof
		    }
     | Axiom { core :: Exp Bool 
	     }
     | Reference { core :: Exp Bool }
    deriving ( Eq, Ord )

{-! for Proof derive: ToDoc, Reader !-}

-- local variables:
-- mode: haskell
-- end;
