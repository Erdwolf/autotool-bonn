{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Modular.Config where

import Network.XmlRpc.Internals

import Autolib.ToDoc
import Data.Typeable

data Config = 
     Config { contents :: String
		}
       deriving Typeable

instance XmlRpcType ( Config ) where
    toValue d = toValue [("contents",toValue (contents d))
			]
    fromValue v = do
		  t <- fromValue v
		  c <- getField "contents" t
		  return $ Config { contents = c }
    getType _ = TStruct

{-! for Config derive: ToDoc !-}

-- local variables:
-- mode: haskell
-- end

