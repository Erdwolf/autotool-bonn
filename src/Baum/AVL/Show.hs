module Baum.AVL.Show where

import Baum.AVL.Type (AVLTree, isLeaf, left, right)
import Data.Tree (Tree, unfoldTree)

{-
toTree :: Show a => AVLTree a -> Tree String
toTree = foldt 
    ( \ l k r -> Node ( show k ) [ l, r ] )
    ( Node "-" [] )
-}

toTree :: Show a => AVLTree a -> Tree String
toTree = unfoldTree uf
    where
      uf t | isLeaf (left t) && isLeaf (right t) = (show x,[])
      uf t |                    isLeaf (right t) = (show x,[left t])
      uf t | isLeaf (left t)                     = (show x,[right t])
      uf t |                                     = (show x,[left t, right t])




        
