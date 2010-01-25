{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

module Baum.AVL.Type where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
     
data AVLTree a = Leaf 
                | Branch { left :: AVLTree a ,
                           key :: a,
                           weight :: Int ,
                           right :: AVLTree a }
     deriving ( Typeable, Eq )

$(derives [makeReader, makeToDoc] [''AVLTree])

instance Functor AVLTree where
  fmap f = foldt (\ l k w r -> Branch l (f k) w r ) Leaf

        
isLeaf :: AVLTree a -> Bool
isLeaf Leaf = True
isLeaf _    = False

foldt :: (a -> b -> Int -> a -> a) -> a -> AVLTree b -> a
foldt branch leaf t = case t of
      Leaf           -> leaf
      Branch l k w r -> branch (foldt branch leaf l)
                                k
                                w
                               (foldt branch leaf r)

-- local variables:
-- mode: haskell
-- end;




