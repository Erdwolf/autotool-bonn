{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

-- | Interface für externe Korrektoren

module Inter.Action where

import Control.Types

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

import Network.XmlRpc.THDeriveXmlRpcType
import Network.XmlRpc.Internals

-- | die Nummer dessen, der sich einloggt (Tutor)
data Actor
     = Actor { schule   :: String
	     , matrikel :: String
	     , passwort :: String 
	     }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Actor])

$(asXmlRpcStruct ''Actor)

data Problem
     = Problem { vorlesung :: String 
	       , aufgabe :: String
	       }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Problem])

$(asXmlRpcStruct ''Problem)



data Answer =
     Answer { wert :: Wert
	    , kommentar :: String
	    }
     deriving ( Eq, Ord, Typeable )

$(asXmlRpcStruct ''Answer)

$(derives [makeReader, makeToDoc] [''Answer])

instance XmlRpcType Wert where
    toValue w = case w of
        Reset   -> toValue [("tag", toValue "Reset")]
        Pending -> toValue [("tag", toValue "Pending")]
        No -> toValue [("tag", toValue "No")]
        Ok s -> toValue [("tag", toValue "OK"), ("size", toValue s) ]
	Okay {} -> toValue [("tag", toValue "Okay")
                  , ("punkte", toValue $ punkte w ), ("size", toValue $ size w) ]
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
	       return $ okay p s
    getType _ = TStruct

-- FIXME
instance XmlRpcType Integer where
    toValue = toValue . ( fromIntegral :: Integer -> Int ) 
    fromValue = fmap ( fromIntegral :: Int -> Integer ) . fromValue
    getType _ = TInt -- FIXME

-- Local Variables:
-- mode: haskell
-- End:
