{-# OPTIONS -fallow-overlapping-instances -fth #-}

module Control.TH where


import Inter.Crypt
import Control.Types

import Network.XmlRpc.THDeriveXmlRpcType
import Network.XmlRpc.Internals

-- should be overridden by more specific instances
instance ( Read a, Show a ) => XmlRpcType a where
    toValue i = toValue ( show i )
    fromValue v = do s <- fromValue v ; return $ read s 
    getType _ = TString

instance XmlRpcType a => XmlRpcType ( Maybe a ) where
    toValue Nothing    = ValueArray [ toValue "Nothing"             ]
    toValue ( Just x ) = ValueArray [ toValue "Just"    , toValue x ]
    fromValue ( ValueArray [ _ ] ) = return $ Nothing
    fromValue ( ValueArray [ _, x ] ) = do a <- fromValue x ; return $ Just a
    getType _ = TArray

instance ( XmlRpcType a, XmlRpcType b ) => XmlRpcType ( a, b ) where
    toValue ( a, b ) = ValueArray [ toValue a, toValue b ]
    fromValue v = do
        ValueArray [ a, b ] <- return v
        x <- fromValue a
        y <- fromValue b
        return ( x, y )
    getType _ = TArray

-- Strings

instance XmlRpcType Email where
    toValue ( Email s ) = toValue s
    fromValue v = do s <- fromValue v ; return $ Email s
    getType _ = TString

instance XmlRpcType Name where
    toValue ( Name s ) = toValue s
    fromValue v = do s <- fromValue v ; return $ Name s
    getType _ = TString

$(asXmlRpcStruct ''Crypt)

--  $(asXmlRpcStruct ''Wert)

-- | wieso steht das hier?
instance XmlRpcType Wert where
    toValue w = case w of
        Reset   -> toValue [("tag", toValue "Reset")]
        Pending -> toValue [("tag", toValue "Pending")]
        No -> toValue [("tag", toValue "No")]
        Ok s -> toValue [("tag", toValue "OK"), ("size", toValue s) ]
	Okay {punkte=p, size=s} -> toValue [("tag", toValue "Okay")
                  , ("punkte", toValue p), ("size", toValue s) ]
    fromValue v = do
        it <- fromValue v
	tag <- getField "tag" it
	case tag of
	    "Reset" -> return Reset
	    "Pending" -> return Pending
	    "No" -> return No
	    "Ok" -> do s <- getField "size" it ; return $ ok s
	    "Okay" -> do 
               p <- getField "punkte" it; s <- getField "size" it
	       return $ Okay { punkte = p , size = s }
    getType _ = TStruct



-- Show

instance XmlRpcType MNr where
    toValue ( MNr s ) = toValue $ show s
    fromValue v = do s <- fromValue v ; return $ MNr $ read s
    getType _ = TInt

-- Ints

instance XmlRpcType ANr where
    toValue ( ANr s ) = toValue s
    fromValue v = do s <- fromValue v ; return $ ANr s
    getType _ = TInt

instance XmlRpcType GNr where
    toValue ( GNr s ) = toValue s
    fromValue v = do s <- fromValue v ; return $ GNr s
    getType _ = TInt

instance XmlRpcType VNr where
    toValue ( VNr s ) = toValue s
    fromValue v = do s <- fromValue v ; return $ VNr s
    getType _ = TInt

instance XmlRpcType SNr where
    toValue ( SNr s ) = toValue s
    fromValue v = do s <- fromValue v ; return $ SNr s
    getType _ = TInt

instance XmlRpcType UNr where
    toValue ( UNr s ) = toValue s
    fromValue v = do s <- fromValue v ; return $ UNr s
    getType _ = TInt


