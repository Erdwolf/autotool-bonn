{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

module Baum.List.Type 

( ListTree, branch, leaf -- smart constructors
, isLeaf
, left, right, key
, height, weight
, foldt, inorder
, proper, correct_weights, correct_heights, small_weights
)


where

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Data.Typeable


data ListTree a = Nil
                | Cons a ( ListTree a )
     deriving ( Typeable, Eq )

------------------------------------------------------

proper :: Ord a => ListTree a -> Bool
proper t = ordered_keys t
        && correct_weights t
        && correct_heights t
        && small_weights t

ordered_keys :: Ord a => ListTree a -> Bool
ordered_keys = monotone . inorder

for_all_subtrees :: ( ListTree a -> Bool )
                 -> ListTree a -> Bool
for_all_subtrees p t = p t && 
    case t of 
      Branch {} -> for_all_subtrees p ( left t )
                && for_all_subtrees p ( right t )
      Leaf {} -> True

small_weights :: ListTree a -> Bool
small_weights = for_all_subtrees $ \ t ->
    abs ( weight t ) <= 1

correct_heights :: ListTree a -> Bool
correct_heights = for_all_subtrees $ \ t -> case t of
    Leaf {} -> True
    Branch {} -> height t == succ (
        max ( height $ left t ) ( height $ right t ) )

correct_weights  :: ListTree a -> Bool
correct_weights = for_all_subtrees $ \ t -> case t of
        Leaf {} -> True
        Branch {} -> weight t == 
           height ( right t ) - height ( left t )


inorder :: ListTree a -> [a]
inorder = foldt ( \ l k r -> l ++ [k] ++ r ) []

monotone :: Ord a => [a] -> Bool
monotone xs = and $ zipWith (<=) xs $ tail xs

---------------------------------------------------

instance Functor ListTree where
  fmap f = foldt (\ l k r -> branch l (f k) r ) leaf

isLeaf :: ListTree a -> Bool
isLeaf ( Leaf {} ) = True
isLeaf _    = False

foldt :: (a -> b -> a -> a) 
      -> a 
      -> ListTree b 
      -> a
foldt branch leaf t = case t of
      Leaf {}        -> leaf
      Branch {left = l, key = k, right = r } -> 
          branch (foldt branch leaf l)
                                k
                               (foldt branch leaf r)

instance Size ( ListTree a ) where
    size = foldt ( \ l _ r ->  l + 1 + r ) 0


$(derives [makeReader, makeToDoc] [''ListTree])





