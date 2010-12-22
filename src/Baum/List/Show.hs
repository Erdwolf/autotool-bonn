module Baum.List.Show where

import Baum.List.Type
import Data.Tree

toTree :: Show a => ListTree a -> Tree String
toTree l = case l of
    Cons x xs -> Node ( show x ) [ toTree xs ]
    Nil       -> Node "-" [] 




        