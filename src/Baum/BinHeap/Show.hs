module Baum.BinHeap.Show where

import Baum.BinHeap.Type
import Data.Tree

toTree :: Show a => BinHeap a -> Tree String
toTree t = Data.Tree.Node "uh" [] 
