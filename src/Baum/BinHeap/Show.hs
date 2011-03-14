module Baum.BinHeap.Show where

import Baum.BinHeap.Type
import Data.Tree

toTree :: Show a => BinHeap a -> Tree String
toTree = Data.Tree.Node "-" 
       . map ( tfold $ \ k cs -> 
                  Data.Tree.Node ( show k ) cs )
       . roots 


