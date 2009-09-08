{-# OPTIONS -fallow-overlapping-instances #-}

module Modular.Seed where

import Network.XmlRpc.Internals

import Autolib.ToDoc

data Seed = 
     Seed { contents :: Int
	      }

instance XmlRpcType ( Seed ) where
    toValue d = toValue [("contents",toValue (contents d))
			]
    fromValue v = do
		  t <- fromValue v
		  c <- getField "contents" t
		  return $ Seed { contents = c }
    getType _ = TStruct

{-! for Seed derive: ToDoc !-}

-- local variables:
-- mode: haskell
-- end

