module Baum.ZweiDrei.Show where

--  $Id$

import Baum.ZweiDrei.Type
import Data.Tree

toTree :: Show a => Baum a -> Tree String
toTree Null = Node "-" []
toTree ( Baum bks) = 
    let keys = do ( _, This k ) <- bks ; return k
        children = do ( b, _ ) <- bks ; return b
    in  Node ( show keys ) $ map toTree children 




        