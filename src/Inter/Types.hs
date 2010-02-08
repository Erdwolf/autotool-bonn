{-# LANGUAGE GADTs #-}

module Inter.Types where


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

type Matrikel = String

type Key = String

dashed :: Show p => p -> String
dashed p = map ( \ c -> if isSpace c then '-' else c ) 
	 $ unwords $ words $ show  p

-- | Make name (maker function) example
data Make = forall conf p i b 
          . ( V p i b 
	    , Typeable conf 

	    , ToDoc conf -- 
	    -- , Show conf
	    , Reader conf -- 
	    -- , Read conf
	    , Help conf, Help i, Help b
	    , Verify p conf
	    )
	  => Make p
                  String --  description
		  (conf -> Var p i b) --  maker function
		  (conf -> Reporter ()) -- verify config
                  conf --  example

---------------------------------------------------------------------------------

{-
instance ( Reader a, ToDoc a, Typeable a ) => XmlRpcType a where 
    getType x = TString
    toValue x = ValueString $ show $ toDoc x
    fromValue  = \ v -> case v of
       ( ValueString s ) ->
	    case parse ( parsec_wrapper 0 ) "input" s of
		 Right (x, []) -> return x
		 res   -> fail $ "parse error, input: " ++ s
       _ -> fail 
              $ "using (wrong) default XmlRpcType instance for " 
                  ++ show (typeOf ( undefined :: a ))
-}

-- the XmlRpcType(a,b) instance is now in Autolib.XmlRpc
-- the XmlRpcType Integer instance is now in Autolib.XmlRpc


---------------------------------------------------------------------------------

instance Typeable Make where 
    typeOf _ = mkTyConApp ( mkTyCon "Inter.Types.Make" ) []

instance ToDoc Make 
    where toDoc ( Make p doc fun veri ex ) = text doc

-- | build maker just from Challenger.Partial instance
-- (suitable for simple problems that don't need generation of instances)
direct :: ( V p i b 
	  , Reader i 
	  -- , Read i 
	  , ToDoc i
	  , Help i, Help b
	  , Verify p i
	  )
         => p 
	 -> i -- ^ example instance
	 -> Make
direct p i = Make p
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
	     -- | erzeugt cached version der instanz (o. ä.)
	     , key :: Matrikel -> IO Key

	     -- | holt tatsächliche instanz
	     -- TODO: der Reporter hier ist unsinnig,
	     -- das würfeln soll schweigend gehen,
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

get_i_type :: Var p i b -> i
get_i_type = error "get_i_type: don't call"


get_b_type :: Var p i b -> b
get_b_type = error "get_b_type: don't call"

class ( Show p, Typeable p , Read p
      , Typeable i, ToDoc i
      , Reader i, Read i
      , Typeable b
      , ToDoc b, Reader b 
      , Partial p i b
      , OrderScore p
      ) => V p i b -- ohne methoden
instance ( Show p, Typeable p, Read p
      -- , Roller i i
      , Typeable i, ToDoc i
      , Reader i, Read i
      , Typeable b
      , ToDoc b, Reader b 
      , Partial p i b
      , OrderScore p
      ) => V p i b -- ohne methoden


data Variant = forall p i b 
         . V p i b
	 => Variant ( Var p i b )

instance ToDoc Variant where 
    toDoc ( Variant v ) = -- text "Variant" -- <+> parens ( toDoc v )
        text $ tag v

------------------------------------------------------------------------------
-- sorting order for toplists

data ScoringOrder = None | Increasing | Decreasing

class OrderScore p where
    scoringOrder :: p -> ScoringOrder

instance OrderScore Make where
    scoringOrder (Make p _ _ _ _) = scoringOrder p
