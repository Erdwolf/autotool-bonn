module Code.Quiz where

--  $Id$

import Code.Type
import Code.Class

import Inter.Types

import Random
import Autolib.Util.Wort
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Util.Size
import Autolib.Util.Seed

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

data Config a b =
     Config { alphabet :: Set a
	    , length_range :: ( Int, Int )
	    , coder :: Coder a b
	    }
     deriving ( Typeable )

throw :: ( Typeable a, Typeable b )
      => Config a b -> IO [a]
throw conf = do
    l <- randomRIO $ length_range conf
    w <- someIO ( setToList $ alphabet conf ) l
    return w


enc :: ( Ord a, Show a, ToDoc [a], Reader [a]
       , Show b, ToDoc b, Reader b , Size b, Eq b
       , Typeable a , Typeable b
       , Haskell2Xml a, Haskell2Xml b
       )
     => Config a b
     -> IO Variant
enc conf = return $ Variant 
    $ Var { problem = Encode ( coder conf )
	  , tag = "enc" ++ ( render $ nametag $ coder conf ) ++ "-" ++ "Quiz"
	  , key = \ matrikel -> return matrikel
	  , gen = \ key -> do
		seed $ read key
	        input <- throw conf
		return $ return input
	  }

dec :: ( Ord a, Show a, ToDoc [a], Reader [a], Size a
       , Show b, ToDoc b, Reader b , Size b, Eq b
       , Typeable a, Typeable b
       , Haskell2Xml a, Haskell2Xml b
       )
     => Config a b
     -> IO Variant
dec conf = return $ Variant 
    $ Var { problem = Decode ( coder conf )
	  , tag = "dec" ++ ( render $ nametag $ coder conf ) ++ "-" ++  "Quiz"
	  , key = \ matrikel -> return matrikel
	  , gen = \ key -> do
		seed $ read key
	        input <- throw conf
		let output = encode (coder conf) input
		return $ return output
	  }

