module Code.Huffman.Quiz 

( make
, Config (..)
, module Code.Type 
)

where

--  $Id$

import Code.Type
import Code.Huffman.Partial
import Code.Huffman.Throw

import Autolib.Util.Seed
import Inter.Types
import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable
import Text.XML.HaXml.Haskell2Xml

make :: ( Ord a
	, Reader a
	, Show a, ToDoc a, ToDoc [a]
	, Typeable a
	, Haskell2Xml a
	) => Config a 
	  -> IO Variant
make conf = return $ Variant 
    $ Var { problem = Huffman
	  , aufgabe = show Huffman
	  , version = "Quiz"
	  , key = \ matrikel -> return matrikel
	  , gen = \ key -> do
		seed $ read key
	        freq <- throw conf
		return $ return freq
	  }
