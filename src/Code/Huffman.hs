module Code.Huffman 

( make
)

where

--  $Id$

import Code.Type
import Code.Huffman.LR
import Code.Measure

import Util.Sort
import Data.FiniteMap
import Reporter
import ToDoc

isoptimalprefix ::  ( ToDoc [b], ToDoc [a], ToDoc a, Ord a, Eq b )
      => Frequency a
      -> Code a b
      -> Reporter ()
isoptimalprefix freq code = do
    inform $ vcat 
	   [ text "Ist der Code" 
	   , nest 4 $ toDoc code
	   , text "ein optimaler Präfix-Code für die Verteilung"
	   , nest 4 $ toDoc freq
	   , text "?"
	   ]
    let mcode = measure freq code
    inform $ text "Der Code hat das Gesamtgewicht" <+> toDoc mcode

    let huff = make freq
	mhuff = measure freq huff

    when ( mcode > mhuff ) $ reject
	   $ text "Das ist zu groß."
    inform $ text "Das ist optimal."

-- | construct Huffman code
make :: Ord a
     => Frequency a 
     -> Code a LR
make xis = 
    let [ top ] = maker $ do 
	   (x, i) <- fmToList xis
	   return $ Letter { weight = i, codes = listToFM [(x, [])] }
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
	   , codes  = listToFM $ do
	         ( c, it ) <- [ (L, x), (R, y) ]
		 ( z, cs ) <- fmToList $ codes it
		 return ( z, c : cs )
	   }

