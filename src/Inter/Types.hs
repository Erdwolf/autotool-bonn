module Inter.Types where

--   $Id$

import Autolib.Reporter

import Challenger.Problem
import Challenger.Partial

-- import ShowFunctions

import  Autolib.ToDoc
import  Autolib.Reader
import  Autolib.Size
import  Autolib.Informed

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

type Matrikel = String

type Key = String

-- | Make name (maker function) example
data Make = forall conf p i b 
          . ( V p i b 
	    , Typeable conf, Haskell2Xml conf, ToDoc conf, Show conf, Reader conf
	    )
	  => Make String --  description
		  (conf -> Var p i b) --  maker function
                  conf --  example

-- | build maker just from Challenger.Partial instance
-- (suitable for simple problems that don't need generation of instances)
direct :: ( V p i b	    )
         => p 
	 -> i -- ^ example instance
	 -> Make
direct p i = Make 
             (show p ++ "-Direct") 
	     ( \ i -> Var { problem = p
		   , aufgabe = show p
		   , version = "-Direct"
		   , key = \ mat -> return mat
		   , gen = \ key -> return $ return i
		   }
	     )
             i

data Var p i b = 
         Var { problem :: p 
	     , aufgabe :: String
	     , version :: String
	     -- erzeugt cached version der instanz (o. ä.)
	     , key :: Matrikel -> IO Key

	     -- holt tatsächliche instanz
	     -- TODO: der Reporter hier ist unsinnig,
	     -- das würfeln soll schweigend gehen,
	     -- wenn es etwas zu sagen gibt,
	     -- dann soll es mit (informed) drangeschrieben werden.
	     , gen :: Key -> IO ( Reporter i )


	     }
      deriving Typeable

--     deriving Show

-- instance ( Show p ) => ToDoc ( Var p i b ) where
--    toDoc = text . show

class ( Show p, Typeable p
      , Show i, Typeable i, Haskell2Xml i, ToDoc i, Reader i
      , Show b, Typeable b, Haskell2Xml b, ToDoc b, Reader b , Size b
      , Partial p i b
      ) => V p i b -- ohne methoden
instance ( Show p, Typeable p
      , Show i, Typeable i, Haskell2Xml i, ToDoc i, Reader i
      , Show b, Typeable b, Haskell2Xml b, ToDoc b, Reader b , Size b
      , Partial p i b
      ) => V p i b -- ohne methoden


data Variant = forall p i b 
         . V p i b
	 => Variant ( Var p i b )

instance ToDoc Variant where 
    toDoc ( Variant v ) = -- text "Variant" -- <+> parens ( toDoc v )
        text $ show ( problem v) ++ ":" ++ aufgabe v ++ "-" ++ version v

instance Show Variant where
    show = render . toDoc



