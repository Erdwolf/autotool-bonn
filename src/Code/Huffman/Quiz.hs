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



make :: ( Ord a
	, Reader a
	, Show a, ToDoc a, ToDoc [a]
	, Typeable a
	) => Config a 
	  -> IO Variant
make conf = return $ Variant 
    $ Var { problem = Huffman
	  , tag = show Huffman ++ "-" ++ "Quiz"
	  , key = \ matrikel -> return matrikel
	  , gen = \ key -> do
		seed $ read key
	        freq <- throw conf
		return $ return freq
	  }
