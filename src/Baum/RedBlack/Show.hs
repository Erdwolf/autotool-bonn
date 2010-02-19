module Baum.RedBlack.Show where

--  $Id$

import Baum.RedBlack.Type
import Data.Tree

import Autolib.Dot.Node

instance Show RedBlackColor where
  show Red = "r"
  show Black = "b"

toTree :: Show a => RedBlackTree a -> Tree String
toTree Empty = Node "E" []
toTree ( RedBlackTree color left key right )
  = Node ( show key ++ show color ) [ toTree ( left ), toTree ( right )]

toTreeNode :: Show a 
           => RedBlackTree a 
           -> Tree Autolib.Dot.Node.Type
toTreeNode Empty = Node ( Autolib.Dot.Node.blank 
                           { label = Just "-" 
                           , shape = Just "plaintext"
                           } ) []

toTreeNode ( RedBlackTree color left key right )
   = Node ( Autolib.Dot.Node.blank { label = Just $ show key 
                  , shape = Just "ellipse"
                  , color = Just $ case color of Red -> "red" ; Black -> "black"
                  } )
          $ map toTreeNode [ left, right ]
