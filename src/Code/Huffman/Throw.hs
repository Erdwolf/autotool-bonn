module Code.Huffman.Throw where

--  $Id$

import Code.Type
import Code.Huffman.Config

import Autolib.Reader
import Autolib.ToDoc

import System.Random


throw :: ( Ord a , ToDoc [a], Reader [a] )
      => Config a 
      -> IO ( Frequency a )
throw conf = do
    xis <- sequence $ do 
        x <- setToList $ alphabet conf
	return $ do
	    i <- randomRIO $ range conf
	    return (x, i)
    return $ Frequency $ listToFM xis

