-- -*- mode: haskell -*-

module Baum.Such.Class where

import Autolib.ToDoc

class Such baum where

    empty :: baum a
    isEmpty :: baum a -> Bool 

    contains :: Ord a => baum a -> a -> Bool
    insert :: Ord a => baum a -> a -> baum a
    delete :: Ord a => baum a -> a -> baum a

    equal :: Eq a => baum a -> baum a -> Bool
    contents :: Ord a => baum a -> [a]
    form :: Show a => baum a -> Doc