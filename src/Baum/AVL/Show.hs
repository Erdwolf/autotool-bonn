module Baum.AVL.Show where

import Baum.AVL.Type as T
import Data.Tree

{-
toTree :: Show a => AVLTree a -> Tree String
toTree = foldt 
    ( \ l k r -> Node ( show k ) [ l, r ] )
    ( Node "-" [] )
-}

toTree :: Show a => AVLTree a -> Tree String
toTree = Data.Tree.unfoldTree uf
    where
      uf (T.Branch T.Leaf x T.Leaf) = (show x,[])
      uf (T.Branch l      x T.Leaf) = (show x,[l])
      uf (T.Branch T.Leaf x r     ) = (show x,[r])
      uf (T.Branch l      x r     ) = (show x,[l,r])




        
