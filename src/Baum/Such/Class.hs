-- -*- mode: haskell -*-

module Baum.Such.Class where

import Baum.Such.Op
import Autolib.ToDoc
import Autolib.Hash
import Data.Tree
import Data.Typeable
import Baum.Dot

class Such baum where

    empty :: baum a
    isEmpty :: baum a -> Bool 

    contains :: Ord a => baum a -> a -> Bool
    insert :: Ord a => baum a -> a -> baum a
    delete :: Ord a => baum a -> a -> baum a

    equal :: Eq a => baum a -> baum a -> Bool
    contents :: Ord a => baum a -> [a]
    toTree :: Show a => baum a -> Tree String

instance ( Such baum, Show a ) => ToDot ( baum a ) where
    toDotProgram = toDotProgram . toTree
    toDotOptions = toDotOptions . toTree
    toDot        = toDot        . toTree

instance Hash a => Hash ( Tree a ) where
    hash ( Node f args ) = hash ( f, hash args )

instance ( Eq (baum a), Such baum, Hash a, Show a ) => Hash ( baum a ) where
    hash = hash . toTree

mirror :: Tree a -> Tree a
mirror ( Node f args ) = Node f ( map mirror args )

form :: ( Such baum, OpC a ) 
     => baum a -> Doc
form = vcat . map text . lines . drawTree . mirror . fmap text . toTree

class ( Show t, ToDoc t, Typeable t
      , Such baum, OpC a
      , ToDoc (baum a), Show ( baum a ), Read ( baum a ) 
      , ToDot ( baum a), Eq (baum a), Hash (baum a)
      , Typeable (baum a)
      ) 
    => Tag t baum a | t -> baum, t -> a where
   tag :: t



