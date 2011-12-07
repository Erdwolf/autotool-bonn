module Baum.AVL.Show where

import Baum.AVL.Type (AVLTree, isLeaf, left, right, key)
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
      uf t | isLeaf l && isLeaf (right t) = (x,[])
           |                    isLeaf (right t) = (x,[left t])
           | isLeaf (left t)                     = (x,[right t])
           |             otherwise               = (x,[left t, right t])
        where x = show (key t)
              l = left t




        
