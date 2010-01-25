module Baum.AVL.Show where

--  $Id$

import Baum.AVL.Type
import Data.Tree

toTree :: Show a => AVLTree a -> Tree String
toTree Leaf = Node "-" []
toTree t = Node ( show $ key t )
                [ toTree (left t), toTree (right t)]




        