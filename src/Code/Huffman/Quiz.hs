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

import Inter.Types

make :: Config a -> IO Variant
make conf = return $ $ Variant 
    $ Var { problem = Huffman
	  , aufgabe = show Huffman
	  , version = "Quiz"
	  , key = \ matrikel -> return matrikel
	  , gen = \ key -> do
	        freq <- throw conf
		return $ return freq
	  }
