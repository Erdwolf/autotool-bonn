{-# OPTIONS -fallow-overlapping-instances #-}

module Modular.Task where

import Autolib.ToDoc
import Network.XmlRpc.Internals

data Task = 
     Task { contents :: String
		}

instance XmlRpcType ( Task ) where
    toValue d = toValue [("contents",toValue (contents d))
			]
    fromValue v = do
		  t <- fromValue v
		  c <- getField "contents" t
		  return $ Task { contents = c }
    getType _ = TStruct


{-! for Task derive: ToDoc !-}

-- local variables:
-- mode: haskell
-- end



