module Baum.BinHeap.Ops where

import Baum.BinHeap.Type
import Data.List hiding (insert)
-- import Autolib.Size

empty :: BinHeap a
empty = Empty {}

isEmpty :: BinHeap a -> Bool
isEmpty xs = case xs of
				Empty -> True
				_	  -> False
				
equal :: Eq a => BinHeap a -> BinHeap a -> Bool
equal xs ys = if (xs == ys) then
				True
			  else
			  	False
			  	
toList :: BinHeap a -> [(Position,a)]
toList t = case t of
			Empty -> []
			_	  -> foldl1 (++) (map (\n -> toListTree n) (roots t))

contents :: Ord a => BinHeap a -> [a]
contents t = case t of
			  Empty -> []
			  _	    -> sort $ foldl1 (++) (map (\n -> contentsTree n) (roots t))
			  	
-- | Einfügen eines Schlüssels in einen Heap
insert :: Ord a => BinHeap a -> a -> BinHeap a
insert xs y = case xs of
				Empty -> Trees { roots = [Node {ord = 0, key = y, pos=Pos{posi=0}, children = []}]}
				_	  -> Trees { roots = (automerge (sortBy sortByOrd ((roots xs)++[Node {ord = 0, key = y, pos=Pos{posi=(foldl1 (max) (map (\n -> getMax n) (roots xs)) + 1)}, children = []}])))}
							-- | den Root-Nodes wird das neue Element hinzugefügt und die entstehende Liste nach
							-- | Ordnung der Knoten sortiert. Alle Knoten gleicher Ordnung werden gemerged.
----------------------------------------------------------------------------------------------------------------------------
inserts :: Ord a => BinHeap a -> [a] -> BinHeap a
inserts xs ys = case ys of
					y:ys' ->	inserts (insert xs y) ys'
					[]	  ->	xs
----------------------------------------------------------------------------------------------------------------------------
-- | Feststellen und Zurückgegeben des Knotens mit dem minimalen Schlüssel sowie der Restliste, zudem Entfernung des
-- | Knotens aus dem Heap
deleteMin :: Ord a => BinHeap a -> BinHeap a
deleteMin xs = case xs of
					Empty -> error "No elements in list!"
					_	  -> let ks = remove (roots xs) (head (sortBy sortByKey (roots xs))) in
							 Trees { roots = (automerge (sortBy sortByOrd (ks++(children (head (sortBy sortByKey (roots xs)))))))}

decreaseTo :: Ord a => BinHeap a -> Position -> a -> BinHeap a
decreaseTo xs a b = Trees { roots = (map (\n -> decrease n (posi a) b) (roots xs))}
