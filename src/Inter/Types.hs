{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

module Inter.Types where

--   $Id$

import Autolib.Reporter
import Gateway.Help

-- import Challenger.Problem
import Challenger.Partial

-- sollte man nicht importieren, enth�lt aber VNr und MNr
import Control.Types 

import  Autolib.ToDoc
import  Autolib.Reader
-- import  Autolib.Size
import  Autolib.Informed

import Data.Typeable
import Data.Char

-- import Text.XML.HaXml.Haskell2Xml
-- import Network.XmlRpc.Internals

type Matrikel = String

type Key = String

dashed :: Show p => p -> String
dashed p = map ( \ c -> if isSpace c then '-' else c ) 
	 $ unwords $ words $ show  p

-- | Make name (maker function) example
data Make = forall conf p i b 
          . ( V p i b 
	    , Typeable conf 

	    -- , Haskell2Xml conf
	    -- , XmlRpcType conf

	    , ToDoc conf , Show conf
	    , Reader conf , Read conf
	    , Help conf, Help i, Help b
	    , Verify p conf
	    )
	  => Make String --  description
		  (conf -> Var p i b) --  maker function
		  (conf -> Reporter ()) -- verify config
                  conf --  example

instance Typeable Make where 
    typeOf _ = mkTyConApp ( mkTyCon "Inter.Types.Make" ) []

instance ToDoc Make 
    where toDoc ( Make doc fun veri ex ) = text doc

-- | build maker just from Challenger.Partial instance
-- (suitable for simple problems that don't need generation of instances)
direct :: ( V p i b 
	  -- , Haskell2Xml i
	  -- , XmlRpcType i
	  , Reader i , Read i , Show i
	  , Help i, Help b
	  , Verify p i
	  )
         => p 
	 -> i -- ^ example instance
	 -> Make
direct p i = Make 
             (dashed p ++ "-Direct") 
	     ( \ i -> Var { problem = p
		   , tag = dashed p ++ "-Direct"
		   , key = \ mat -> return mat
		   , gen = \ vnr manr key -> return $ return i
                   , generate = \ salt -> return $ return i
		   }
	     )
	     ( verify p )
             i

data Var p i b = 
         Var { problem :: p 
	     -- | das ist der unique bezeichner 
	     -- (den der benutzer in auswahlliste sieht)
	     -- gar nicht mehr wahr!
	     , tag :: String
	     -- | erzeugt cached version der instanz (o. �.)
	     , key :: Matrikel -> IO Key

	     -- | holt tats�chliche instanz
	     -- TODO: der Reporter hier ist unsinnig,
	     -- das w�rfeln soll schweigend gehen,
	     -- wenn es etwas zu sagen gibt,
	     -- dann soll es mit (informed) drangeschrieben werden.

	     -- FIXME: hier werden VNr und ANr benutzt,
	     -- das darf aber nicht sein (man soll hier gar nicht wissen,
	     -- da� es so etwas gibt)
	     , gen :: VNr -> Maybe ANr -> Key 
                   -> IO ( Reporter i )

             -- so soll es sein: eingabe ist seed f�r Zufall
             , generate :: Int
                        -> IO ( Reporter i )

	     }
      deriving Typeable

class ( Show p, Typeable p , Read p
      , Typeable i, ToDoc i
      -- , Haskell2Xml i
      -- , Reader i, Read i
      , Typeable b
      -- , Haskell2Xml b
      , ToDoc b, Reader b 
      , Partial p i b
      ) => V p i b -- ohne methoden
instance ( Show p, Typeable p, Read p
      -- , Roller i i
      , Typeable i, ToDoc i
      -- , Haskell2Xml i
      -- , Reader i, Read i
      , Typeable b
      -- , Haskell2Xml b
      , ToDoc b, Reader b 
      , Partial p i b
      ) => V p i b -- ohne methoden


data Variant = forall p i b 
         . V p i b
	 => Variant ( Var p i b )

instance ToDoc Variant where 
    toDoc ( Variant v ) = -- text "Variant" -- <+> parens ( toDoc v )
        text $ tag v





