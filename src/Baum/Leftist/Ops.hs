module Baum.Leftist.Ops where

import Baum.Leftist.Type 
import qualified Baum.Heap.Class as C
import Baum.Heap.Op (Position)


instance C.Heap LeftistTree where

    -- empty :: baum a
	empty = Leaf
    
    -- isEmpty :: baum a -> Bool 
	isEmpty t = case t of
		Leaf -> True  
		_ -> False

    -- insert :: Ord a => baum a -> a -> baum a
	insert t k = meld t (Branch Leaf k Leaf)

    -- deleteMin :: Ord a => baum a -> baum a
	deleteMin t = meld (left t) (right t)

        get t ps = case t of
            Leaf -> Nothing
            Branch {} -> case ps of
                [] -> return $ key t
                0 : ps -> C.get ( left  t ) ps
                1 : ps -> C.get ( right t ) ps
                _ -> Nothing

    -- decreaseTo :: Ord a => baum a -> Position -> a -> baum a
	decreaseTo = decreaseTo

    -- equal :: Eq a => baum a -> baum a -> Bool
	equal t1 t2 = t1 == t2

        toList = toList

-- | fuegt zwei Baeume zusammen
meld :: Ord a => LeftistTree a -> LeftistTree a -> LeftistTree a
meld t Leaf = t
meld Leaf t = t
meld t1 t2 = if (key t1) < (key t2) 
	then meld t2 t1 
	else swap (Branch (left t2) (key t2) (meld t1 (right t2)))
		
-- | vertauscht rechten und linken Teilbaum, wenn rechter Baum hoeher
swap :: Ord a => LeftistTree a -> LeftistTree a
swap Leaf = Leaf
swap (Branch l k r) = if (s l) >= (s r) 
	then Branch l k r 
	else Branch r k l 

-- | liefert den kuerzesten Weg bis zum ersten Blatt
s :: LeftistTree a -> Int
s t = case t of
	Leaf -> 0
	_ -> s (right t) + 1

-- | Key in einem Branch verringern und evtl. Aufsteigen lassen
decreaseTo :: Ord a => LeftistTree a -> Position -> a -> LeftistTree a
decreaseTo t [] a = Branch {left = left t, key = a, right = right t}
decreaseTo t (0:pt) a = let x = decreaseTo (left t) pt a
				in f x t 0
decreaseTo t (1:pt) a = let x = decreaseTo (right t) pt a
				in f x t 1

-- | Realisiert das Aufsteigen eines geaenderten Wertes
f :: Ord a => LeftistTree a -> LeftistTree a -> Int -> LeftistTree a
f t1 t2 0 = if (key t1) < (key t2)
		then Branch{ left = Branch{ left = left t1, 
						    key = key t2, 
						    right = right t1},
				 key = key t1, 
				 right = right t2}
		else Branch{ left = t1,
				 key = key t2,
				 right = right t2}
f t1 t2 1 = if (key t1) < (key t2)
		then Branch{ left = left t2,
				 key = key t1, 
				 right = Branch{ left = left t1, 
						     key = key t2, 
						     right = right t1}}
		else Branch{ left = left t2,
				 key = key t2,
				 right = t1}

-- | Erzeugt Liste des Baums mit (Position,Key)
toList :: LeftistTree a -> [(Position, a)]
toList t = case t of
	Leaf -> []
	_ -> toListCount t []

-- | Durch toList initiiert, erzeugt die Liste, bestimmt Positionen
toListCount :: LeftistTree a -> Position -> [(Position, a)]
toListCount t k = case t of
	Leaf -> []
	_ -> [(k, key t)] ++ toListCount (left t) (k ++ [0]) ++ toListCount (right t) (k ++[1])