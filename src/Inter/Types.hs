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

data Var p i b = 
         Var { problem :: p 
	     , variant :: String
	     , key :: Matrikel -> IO Key
	     , gen :: Matrikel -> Reporter i
	     , gen_i :: Key -> i
	     }
     deriving Show

instance ( Show p ) => ToDoc ( Var p i b ) where
    toDoc = text . show

class ( Show p 
	   , Show i
	   , Show b, ToDoc b, Reader b , Size b
	   , Partial p i b
	   ) => V p i b -- ohne methoden
instance ( Show p 
	   , Show i
	   , Show b, ToDoc b, Reader b , Size b
	   , Partial p i b
	   ) => V p i b 


data Variant = forall p i b 
         . V p i b
	 => Variant ( Var p i b )

instance ToDoc Variant where 
    toDoc ( Variant v ) = -- text "Variant" -- <+> parens ( toDoc v )
        text $ show ( problem v) ++ "-" ++  variant v

instance Show Variant where
    show = render . toDoc

