module Baum.RedBlack.Show where

--  $Id$

import Baum.RedBlack.Type
import Data.Tree

instance Show RedBlackColor where
  show Red = "r"
  show Black = "b"

toTree :: Show a => RedBlackTree a -> Tree String
toTree Empty = Node "E" []
toTree ( RedBlackTree color left key right )
  = Node ( show key ++ show color ) [ toTree ( left ), toTree ( right )]
