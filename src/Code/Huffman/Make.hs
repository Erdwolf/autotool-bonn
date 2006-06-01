module Code.Huffman.Make

( make
)

where

--  $Id$

import Code.Type
import Code.Huffman.LR
import Code.Measure

import Autolib.Util.Sort
import Autolib.FiniteMap
import Autolib.Reporter
import Autolib.ToDoc




-- | construct Huffman code
make :: Ord a
     => Frequency a 
     -> Code a LR
make ( Frequency xis ) = 
    let [ top ] = maker $ do 
	   (x, i) <- fmToList xis
	   return $ Letter 
                  { weight = i
                  , codes = Code $ listToFM [(x, [])] 
                  }
    in	codes top

maker :: Ord a
      =>  [ Letter a ] -> [ Letter a ]
maker xs =
    case sortBy weight xs of
        x : y : rest -> maker $ combine x y : rest
	sonst -> sonst

combine :: Ord a
	=> Letter a -> Letter a -> Letter a
combine x y = 
    Letter { weight = weight x + weight y
	   , codes  = Code $ listToFM $ do
	         ( c, it ) <- [ (L, x), (R, y) ]
                 let Code co = codes it
		 ( z, cs ) <- fmToList co
		 return ( z, c : cs )
	   }

