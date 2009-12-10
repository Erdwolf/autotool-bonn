{-# LANGUAGE TemplateHaskell #-}

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


$(derives [makeToDoc] [''Task])

-- local variables:
-- mode: haskell
-- end



