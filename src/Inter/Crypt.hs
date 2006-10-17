module Inter.Crypt 

( Crypt (..)
, encrypt
, Inter.Crypt.compare
, is_empty, empty
)

where

--  $Id$

import Codec.Encryption.MD5 ( hash )
import Codec.Encryption.Utils ( Octet )

import Control.SQL

import System.Random
import Data.Word
import Data.Typeable
import Autolib.Xml
import Autolib.ToDoc hiding ( empty )
import Autolib.Reader

data Crypt = Crypt { unCrypt :: String }    
    deriving ( Eq, Ord, Typeable )
 
is_empty :: Crypt -> Bool
is_empty = null . unCrypt

empty :: Crypt
empty = Crypt ""

-- brauchen kein quote Quots?
instance ToDoc Crypt where toDoc = text . unCrypt
instance Read Crypt where readsPrec p cs = [(Crypt cs, [])]

instance Reader Crypt where 
    reader = do
        cs <- many letter
	return $ Crypt cs

instance Container Crypt String where
    label _ = "Crypt"
    pack = unCrypt
    unpack = Crypt 


uu_base :: Char
uu_base = 'a'

uudecode :: String -> [ Octet ]
uudecode cs =
    let fun :: Char -> Octet
        fun c = toEnum $ fromEnum c - fromEnum uu_base 
	decode [] = []
	decode [x] = error $ "single octet in decode - should not happen: " 
		           ++ show (cs, map fun cs, x)
        decode (hi : lo : rest) = ( 16 * hi + lo ) : decode rest
    in  decode $ map fun cs

uuencode :: [ Octet ] -> String
uuencode octs = do
    oct <- octs
    let (hi, lo) = divMod oct 16
    x <- [ hi, lo ]
    return $ toEnum $ fromEnum 'a' + fromEnum x

salt_length :: Int
salt_length = 4

-- | generate random salt and encrypt
encrypt :: String -> IO Crypt
encrypt cs = do
    isalt <- sequence $ replicate salt_length $ randomRIO (0, 255 :: Int)
    let salt = map toEnum isalt
    let octs :: [ Octet ]
        octs  = salt ++ map ( toEnum . fromEnum ) cs
    let hocts :: [ Octet ]
        hocts = salt ++ Codec.Encryption.MD5.hash octs
    return $ Crypt $ uuencode hocts

-- | 
compare :: Crypt -- ^ from passwd base (encryption of salt + pwd)
	      -> String -- ^ what the user typed (pwd)
	      -> Bool
compare ( Crypt store ) input =
    let ( salt , cpwd ) = splitAt salt_length $ uudecode store
        cinput = Codec.Encryption.MD5.hash $ salt ++ map ( toEnum . fromEnum ) input
    in  cpwd == cinput
