module Code.Quiz where

--  $Id$

import Code.Type
import Code.Class

import Inter.Types

import Random
import Util.Wort
import ToDoc
import Reader
import Util.Size

data Config a b =
     Config { alphabet :: Set a
	    , length_range :: ( Int, Int )
	    , coder :: Coder a b
	    }

throw :: Config a b -> IO [a]
throw conf = do
    l <- randomRIO $ length_range conf
    w <- someIO ( setToList $ alphabet conf ) l
    return w


enc :: ( Ord a, Show a, ToDoc [a]
       , Show b, ToDoc b, Reader b , Size b, Eq b
       )
     => Config a b
     -> IO Variant
enc conf = return $ Variant 
    $ Var { problem = Encode ( coder conf )
	  , aufgabe = "Encode"
	  , version = render $ nametag $ coder conf 
	  , key = \ matrikel -> return matrikel
	  , gen = \ key -> do
	        input <- throw conf
		return $ return input
	  }

dec :: ( Ord a, Show a, ToDoc [a], Reader [a], Size a
       , Show b, ToDoc b, Reader b , Size b, Eq b
       )
     => Config a b
     -> IO Variant
dec conf = return $ Variant 
    $ Var { problem = Decode ( coder conf )
	  , aufgabe = "Decode"
	  , version = render $ nametag $ coder conf 
	  , key = \ matrikel -> return matrikel
	  , gen = \ key -> do
	        input <- throw conf
		let output = encode (coder conf) input
		return $ return output
	  }

