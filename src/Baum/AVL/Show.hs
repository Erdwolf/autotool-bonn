module Baum.AVL.Show where

import Baum.AVL.Type
import Data.Tree

toTree :: Show a => AVLTree a -> Tree String
toTree = foldt 
    ( \ l k r -> Node ( show k ) [ l, r ] )
    ( Node "-" [] )




        