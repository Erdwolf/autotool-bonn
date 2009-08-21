{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances #-}
{-# LANGUAGE TemplateHaskell #-}

module Modular.Signed 

( Signed, sign, unsign )

where

import Autolib.Hash
import Autolib.ToDoc
import Network.XmlRpc.Internals
import System.Random
import Data.Typeable
import Control.Monad ( when )

data Signed a = 
     Signed { contents :: a 
		, signature :: String
		}
    deriving ( Typeable )

modulus :: Integer
modulus = 314159

sign :: Show a => a -> IO ( Signed a )
sign x = do
    ( salt :: Integer ) <- randomRIO ( 0 , modulus - 1 )
    let val :: Integer
	val = fromIntegral $ hash $ show salt ++ show x 
    return $ Signed 
       { contents = x 
       , signature = show $ modulus * val + salt
       }

consistent :: Show a => Signed a -> Bool
consistent s = 
    let ( val, salt ) = divMod ( read $ signature s ) modulus
	h = fromIntegral $ hash ( show salt ++ show ( contents s ) )
    in  h == val

unsign :: ( Typeable a, Show a ) => Signed a -> IO a
unsign s = do
    when ( not $ consistent s ) 
         $ error $ "wrong signature for " ++ show ( typeOf s )
    return $ contents s

instance XmlRpcType a => XmlRpcType ( Signed a ) where
    toValue d = toValue [("contents",toValue (contents d)),
			 ("signature", toValue (signature d))]
    fromValue v = do
		  t <- fromValue v
		  c <- getField "contents" t
		  d <- getField "signature" t
		  return $ Signed { contents = c, signature = d }
    getType _ = TStruct

$(derives [makeToDoc] [''Signed])

-- local variables:
-- mode: haskell
-- end
