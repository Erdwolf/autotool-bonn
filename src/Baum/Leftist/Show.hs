module Baum.Leftist.Show where

import Baum.Leftist.Type
import Data.Tree

toTree :: Show a => LeftistTree a -> Tree String
toTree l = case l of
	Leaf -> Node "-" []
	Branch l k r -> Node (show k) [toTree l, toTree r]
