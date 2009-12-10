{-# LANGUAGE TemplateHaskell #-}

module Modular.Solution where

import Network.XmlRpc.Internals

import Autolib.ToDoc

data Solution = 
     Solution { contents :: String
		}

instance XmlRpcType ( Solution ) where
    toValue d = toValue [("contents",toValue (contents d))
			]
    fromValue v = do
		  t <- fromValue v
		  c <- getField "contents" t
		  return $ Solution { contents = c }
    getType _ = TStruct

$(derives [makeToDoc] [''Solution])

-- local variables:
-- mode: haskell
-- end

