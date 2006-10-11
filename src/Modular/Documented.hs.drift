{-# OPTIONS -fallow-overlapping-instances #-}

module Modular.Documented where

import Network.XmlRpc.Internals

import Autolib.ToDoc

data Documented a = 
     Documented { contents :: a 
		, documentation :: String 
		}

instance XmlRpcType a => XmlRpcType ( Documented a ) where
    toValue d = toValue [("contents",toValue (contents d)),
			 ("documentation", toValue (documentation d))]
    fromValue v = do
		  t <- fromValue v
		  c <- getField "contents" t
		  d <- getField "documentation" t
		  return $ Documented { contents = c, documentation = d }
    getType _ = TStruct

{-! for Documented derive: ToDoc !-}

-- local variables:
-- mode: haskell
-- end
