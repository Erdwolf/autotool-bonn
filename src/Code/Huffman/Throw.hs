module Code.Huffman.Throw where

--  $Id$

import Code.Type
import Random

data Config a = 
     Config { alphabet :: Set a
	    , range    :: (Int, Int)
	    }

throw :: Ord a 
      => Config a 
      -> IO ( Frequency a )
throw conf = do
    xis <- sequence $ do 
        x <- setToList $ alphabet conf
	return $ do
	    i <- randomRIO $ range conf
	    return (x, i)
    return $ listToFM xis

