module Inter.Crypt where

--  $Id$

import Codec.Encryption.MD5 ( hash )
import Codec.Encryption.Utils ( Octet )

import Random
import Data.Word

uu_base :: Char
uu_base = 'a'

uudecode :: String -> [ Octet ]
uudecode =
    let fun :: Char -> Octet
        fun c = toEnum $ fromEnum c - fromEnum uu_base 
	decode [] = []
        decode (hi : lo : rest) = ( 16 * hi + lo ) : decode rest
    in  decode . map fun

uuencode :: [ Octet ] -> String
uuencode octs = do
    oct <- octs
    let (hi, lo) = divMod oct 16
    x <- [ hi, lo ]
    return $ toEnum $ fromEnum 'a' + fromEnum x

salt_length :: Int
salt_length = 4

-- | generate random salt and encrypt
encrypt :: String -> IO String
encrypt cs = do
    isalt <- sequence $ replicate salt_length $ randomRIO (0, 255 :: Int)
    let salt = map toEnum isalt
    let octs :: [ Octet ]
        octs  = salt ++ map ( toEnum . fromEnum ) cs
    let hocts :: [ Octet ]
        hocts = salt ++ Codec.Encryption.MD5.hash octs
    return $ uuencode hocts

-- | 
compare :: String -- ^ from passwd base (encryption of salt + pwd)
	      -> String -- ^ what the user typed (pwd)
	      -> Bool
compare store input =
    let ( salt , cpwd ) = splitAt salt_length $ uudecode store
        cinput = Codec.Encryption.MD5.hash $ salt ++ map ( toEnum . fromEnum ) input
    in  cpwd == cinput
