module Baum.Binary.Ops where

import Baum.Binary.Type 

contains :: Ord a
	 => Baum a -> a -> Bool
contains Null k = False
contains b k | k == key b = True
	     | k < key b = contains (left b) k
	     | otherwise = contains (right b) k

inter :: Ord a
      => Op
      -> Baum a
      -> Baum a
inter op = case op of
    Insert -> insert 
    Delete -> delete

insert :: Ord a
       => Baum a -> a -> Baum a
insert Null k = Baum { key = k, left = Null, right = Null }
insert b k | k < key b = b { left  = insert (left  b) k }
           | otherwise = b { right = insert (right b) k }

-- | if present, then delete, else return unchanged
delete :: Ord a 
       => Baum a -> a -> Baum a
delete b k 
    | isNull b = b
    | k == key b && isNull (left  b) = right b
    | k == key b && isNull (right b) = left  b
    | k == key b = -- interessanter fall, beide kinder nicht leer
         let ( r, lemo ) = replace_leftmost ( right b ) ( right lemo )
	 in  b { key = key lemo , right = r }

    | k < key b = b { left  = delete (left  b) k }
    | otherwise = b { right = delete (right b) k }

-- replace leftmost node of top with new,
-- return new top and old leftmost
replace_leftmost :: Baum a -> Baum a -> ( Baum a, Baum a )
replace_leftmost top new = case left top of 
    Null -> ( new , top )
    b'   -> let ( b'', lemo ) = replace_leftmost b' new
	    in ( top { left = b'' } , lemo )

 