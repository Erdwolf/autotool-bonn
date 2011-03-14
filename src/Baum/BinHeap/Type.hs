{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

module Baum.BinHeap.Type 

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

data BinHeap a = BinHeap {roots :: [BinTree a]}
	 deriving ( Eq , Typeable )
	 
instance Functor BinHeap where
     fmap f h = BinHeap { roots = map ( fmap f ) $ roots h }


increasingOrders :: BinHeap a -> Bool          
increasingOrders h = 
    and $ zipWith ( \ u v -> order u < order v ) 
                  (roots h ) ( tail $ roots h)

heapIsCorrect :: Ord a => BinHeap a -> Bool
heapIsCorrect h = and 
    $ increasingOrders h : map treeIsCorrect ( roots h )


$(derives [makeReader, makeToDoc] [''BinTree])
$(derives [makeReader, makeToDoc] [''BinHeap])


