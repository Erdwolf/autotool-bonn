{-# OPTIONS -fallow-overlapping-instances #-}

{-# LANGUAGE TemplateHaskell #-}
module Modular.Instance where

import Network.XmlRpc.Internals

import Autolib.ToDoc
import Data.Typeable

data Instance = 
     Instance { tag :: String
              , contents :: String
	      }
    deriving Typeable

instance XmlRpcType ( Instance ) where
    toValue d = toValue [("tag",toValue (tag d))
                        ,("contents",toValue (contents d))
			]
    fromValue v = do
		  t <- fromValue v
		  c <- getField "contents" t
		  a <- getField "tag" t
		  return $ Instance { tag = a, contents = c }
    getType _ = TStruct

$(derives [makeToDoc] [''Instance])
-- {-! for Instance derive: ToDoc !-}

-- local variables:
-- mode: haskell
-- end

