-- -*- mode: haskell -*-

module Baum.Such.Class where

import Baum.Such.Op
import Autolib.ToDoc
import Data.Tree
import Data.Typeable

class Such baum where

    empty :: baum a
    isEmpty :: baum a -> Bool 

    contains :: Ord a => baum a -> a -> Bool
    insert :: Ord a => baum a -> a -> baum a
    delete :: Ord a => baum a -> a -> baum a

    equal :: Eq a => baum a -> baum a -> Bool
    contents :: Ord a => baum a -> [a]
    toTree :: Show a => baum a -> Tree String

form :: ( Such baum, OpC a ) 
     => baum a -> Doc
form = vcat . map text . lines . drawTree . fmap text . toTree

class ( Show t, ToDoc t, Typeable t
      , Such baum, OpC a
      , ToDoc (baum a), Show ( baum a ), Read ( baum a ) 
      , Typeable (baum a)
      ) 
    => Tag t baum a | t -> baum, t -> a where
   tag :: t



