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

data BinTree a = 
     Node { key :: a, children :: [BinTree a]}
     deriving ( Eq, Ord, Typeable )

instance Functor BinTree where
  fmap f = tfold ( \ k cs -> Node ( f k ) cs )

order :: BinTree a -> Int
order t = length $ children t

tfold :: ( a -> [b] -> b ) -> BinTree a -> b
tfold node t = node ( key t ) 
    $ map ( tfold node ) $ children t

subtrees :: BinTree a -> [ BinTree a ]
subtrees t = t : ( children t >>= subtrees )

treeIsOrdered :: Ord  a => BinTree a -> Bool
treeIsOrdered t = and $ do
      u <- subtrees t
      v <- subtrees u
      return $ key u <= key v

treeIsBinomial :: BinTree a -> Bool
treeIsBinomial t = 
      let check (k, t) = 
              and $ ( k == order t ) : do
                  map check $ zip [0 .. ] $ children t
      in  check ( order t , t )
          
treeIsCorrect :: Ord a => BinTree a -> Bool
treeIsCorrect t = treeIsOrdered t && treeIsBinomial t


-------------------------------------------------------- 

data BinHeap a = Trees {roots :: [BinTree a]}
	 deriving ( Eq , Typeable )
	 
instance Functor BinHeap where
     fmap f h = Trees { roots = map ( fmap f ) $ roots h }


increasingOrders :: BinHeap a -> Bool          
increasingOrders h = 
    and $ zipWith ( \(u,v) -> order u < order v ) 
                  (roots h ) ( tail $ roots h)

heapIsCorrect :: Ord a => BinHeap a -> Bool
heapIsCorrect h = and 
    $ increasingOrders h : map treeIsCorrect ( roots h )


-------------------------------------------------------- 

-- | merge order-increasing lists of trees
merge :: Ord a 
      => [BinTree a] -> [ BinTree a ] -> [BinTree a]
merge [] ys = ys      
merge xs [] = xs
merge (x:xs) (y:ys) = case compare ( order x ) ( order y ) of
    LT -> x : merge xs (y:ys)
    GT -> y : merge (x:xs) ys
    EQ -> glue x y : merge xs ys
      
-- | make one tree from two trees of equal order          
glue :: Ord a 
      => BinTree a -> BinTree a -> BinTree a
glue x y | order x == order y =       
    if ( key x < key y ) 
    then x { children = children x ++ [y] }   
    else y { children = children y ++ [x] }         
      


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


