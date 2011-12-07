module Baum.AVL.Show where

import Baum.AVL.Type (AVLTree (Leaf, Branch))
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
      uf (Branch Leaf x Leaf) = (show x,[])
      uf (Branch l    x Leaf) = (show x,[l])
      uf (Branch Leaf x r   ) = (show x,[r])
      uf (Branch l    x r   ) = (show x,[l,r])




        
