{-# LANGUAGE TemplateHaskell #-}

module Modular.Pair where

import Network.XmlRpc.Internals

import Autolib.ToDoc
import Data.Typeable

data Pair a b = 
     Pair { first :: a
	  , second :: b
	  }
     deriving Typeable

instance ( XmlRpcType a, XmlRpcType b ) =>  XmlRpcType ( Pair a b ) where
    toValue d = toValue [ ("first"  , toValue (first  d) )
                        , ("second" , toValue (second d) )
			]
    fromValue v = do
		  t <- fromValue v
		  f <- getField "first" t
		  s <- getField "second" t
		  return $ Pair { first = f, second = s }
    getType _ = TStruct

$(derives [makeToDoc] [''Pair])

-- local variables:
-- mode: haskell
-- end

