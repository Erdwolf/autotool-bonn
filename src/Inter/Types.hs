module Inter.Types where

-- $Id$

import Reporter
import Challenger ( Problem (..), Partial (..) )
import Step

import ShowFunctions
import ToDoc
import Reader
import Size

type Matrikel = String

type Key = String

data Var p i = 
         Var { problem :: p 
	     , variant :: String
	     , key :: Matrikel -> IO Key
	     , gen :: Matrikel -> Reporter i
	     , gen_i :: Key -> i
	     }
     deriving Show

instance ( Show p ) => ToDoc ( Var p i ) where
    toDoc = text . show

data Variant = forall p i b 
         . ( Show p , Reader b , Size b
	   , Problem p i b , Partial p i b
	   )
	 => Variant ( Var p i )

instance ToDoc Variant where 
    toDoc ( Variant v ) = text "Variant" -- <+> parens ( toDoc v )

instance Show Variant where
    show = render . toDoc

