{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-incoherent-instances #-}

module Tree.Class where

--  $Id$

import Tree.Dot

import Data.Tree
import Autolib.Dot
import Autolib.Hash
import Autolib.ToDoc


class ToTree baum where
      toTree :: Show a => baum a -> Tree String

instance ToDot ( Tree String ) where
    toDotProgram t = Dot
    toDotOptions t = unwords [ "-Gordering=out", "-Gnodesep=0" ]
    toDot t = make t 

instance ( ToTree baum, Show a ) => ToDot ( baum a ) where
    toDotProgram = toDotProgram . toTree
    toDotOptions = toDotOptions . toTree
    toDot        = toDot        . toTree

instance Hash a => Hash ( Tree a ) where
    hash ( Node f args ) = hash ( f, hash args )

instance ( Eq (baum a), ToTree baum, Hash a, Show a ) => Hash ( baum a ) where
    hash = hash . toTree

mirror :: Tree a -> Tree a
mirror ( Node f args ) = Node f ( map mirror args )

form :: ( ToTree baum, Show a ) 
     => baum a -> Doc
form = vcat . map text . lines . drawTree . mirror 
     . fmap ( render . toDoc )
     . toTree



