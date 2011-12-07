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
      uf t | isLeaf l && isLeaf r = (show k,[])
           |             isLeaf r = (show k,[l])
           | isLeaf l             = (show k,[r])
           |       otherwise      = (show k,[l,r])
        where k = key t
              l = left t
              r = right t




        
