{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

module Baum.BinHeap.Type 

{-
( BinTree, branch, leaf -- smart constructors
, isLeaf
, left, right, key
, height, weight
, foldt, inorder
, proper, correct_weights, correct_heights, small_weights
)
-}

where

import Autolib.ToDoc hiding (empty)
import Autolib.Reader
import Autolib.Size
import Data.Typeable
import Data.List hiding (insert)

data BinTree a = Null {}
			   | Node {ord :: Int, key :: a, pos :: Position, children :: [BinTree a]}
     deriving ( Eq, Ord, Typeable )

instance Functor BinTree where
     fmap = error "missing instance Functor BinTree"

data BinHeap a = Empty {}
			   | Trees {roots :: [BinTree a]}
	 deriving ( Eq , Typeable )
	 
instance Functor BinHeap where
     fmap = error "missing instance Functor BinHeap"

data Position = Pos {posi :: Int}
	deriving ( Eq, Ord )

isCorrect :: BinHeap a -> Bool
isCorrect t = case t of
				Empty -> True
				_	  -> foldl1 (&&) (map (\n -> check n) (roots t))

check :: BinTree a -> Bool
check t = if(length (children t) > 0) then
			(ord t == length (children t)) && (foldl1 (&&) (map (\n -> check n) (children t)))
		  else
		  	(ord t == length (children t))

-- | y und z werden aus der Liste entfernt, der Knoten mit größerem Schlüssel wird an den anderen Knoten angehängt
-- | und dieser wieder in die Liste eingefügt. Die Liste der Kinder wird hierbei nach der Ordnung absteigend
-- | sortiert.
merge :: Ord a => [BinTree a] -> BinTree a -> BinTree a -> [BinTree a]
merge xs y z = if (key y >= key z) then
				(remove (remove xs y) z)++[Node {ord = (ord z)+1, key = key z, pos=pos z, children = reverse (sortBy sortByOrd(y:(children z)))}]
			   else
			   	(remove (remove xs z) y)++[Node {ord = (ord y)+1, key = key y, pos=pos y, children = reverse (sortBy sortByOrd(z:(children y)))}]

-- | Der gesuchte Knoten wird in der Übergebenen Liste gesucht und, wenn vorhanden, entfernt
remove :: Ord a => [BinTree a] -> BinTree a -> [BinTree a]
remove xs t = case xs of 
				x:xs' -> if (x == t) then
						 	xs'
						 else
							x:(remove xs' t)
				_	  -> xs

-- | Sortierung nach Ordnung
sortByOrd :: Ord a => BinTree a -> BinTree a -> Ordering
sortByOrd a b = if ((ord a) < (ord b)) then LT
		  		else GT

-- | Sortierung nach Schlüssel
sortByKey :: Ord a => BinTree a -> BinTree a -> Ordering
sortByKey a b = if ((key a) <= (key b)) then LT
				else GT
				
getMax :: BinTree a -> Int
getMax t = case t of
			Null -> 0
			_	  -> foldl1 (max) ((posi (pos t)):(map (\n -> getMax n) (children t)))

-- | Automatisches Mergen der Bäume in einem Heap, soweit möglich.
automerge :: Ord a => [BinTree a] -> [BinTree a]
automerge xs = case xs of
				x:xs' -> case xs' of
							x':xs'' ->	if (ord x == ord (head xs')) then
											automerge (sortBy sortByOrd (merge xs x (head xs')))
						 				else
						 					sortBy sortByOrd (x:(automerge xs'))
						 	[]		->	xs
				[]	  -> xs

decrease :: Ord a => BinTree a -> Int -> a -> BinTree a
decrease n a b = if ((posi (pos n)) == a) then
					Node {key=b,ord=(ord n),pos=(pos n), children=(children n)}
				 else
					let tmp = (map (\m -> manageNodes m a b (key n)) (children n)) in
					if (((map (\(o,m) -> m) tmp) /= []) && foldl1 (||) (map (\(o,m) -> m) tmp)) then
						Node {key=b, ord=(ord n), pos=(pos n), children=(map (\(o,m) -> o) tmp)}
					else
						Node {key=(key n), ord=(ord n), pos=(pos n), children=(map (\(o,m) -> o) tmp)}


					
manageNodes :: Ord a => BinTree a -> Int -> a -> a -> (BinTree a, Bool)
manageNodes n a b c = if ((posi (pos n)) == a) then
						if (b < c) then
							(Node {key=c,ord=(ord n),pos=(pos n),children=(children n)}, True)
						else
							(Node {key=b,ord=(ord n),pos=(pos n),children=(children n)}, False)
					  else
						let tmp = (map (\m -> manageNodes m  a b (key n)) (children n)) in
						if (((map (\(o,m) -> m) tmp) /= []) && foldl1 (||) (map (\(o,m) -> m) tmp)) then
							if(b < c) then
								(Node {key=c,ord=(ord n),pos=(pos n),children=(map (\(o,m) -> o) tmp)}, True)
							else
								(Node {key=b,ord=(ord n),pos=(pos n),children=(map (\(o,m) -> o) tmp)}, False)
						else
							(Node {key=(key n), ord=(ord n),pos=(pos n),children=(map (\(o,m) -> o) tmp)}, False)
							
toListTree :: BinTree a -> [(Position, a)]
toListTree t = if(length (children t) > 0) then
			   	   foldl (++) [(pos t, key t)] (map (\n -> toListTree n) (children t))
			   else
			   	   [(pos t, key t)]

contentsTree :: BinTree a -> [a]
contentsTree t = if(length (children t) > 0) then
			   	   foldl (++) [key t] (map (\n -> contentsTree n) (children t))
			   else
			   	   [key t]


$(derives [makeReader, makeToDoc] [''BinTree])
$(derives [makeReader, makeToDoc] [''BinHeap])
$(derives [makeReader, makeToDoc] [''Position])

