{-# LANGUAGE OverlappingInstances #-}

module Tree.Class where

import Tree.Dot

import Data.Tree
import Autolib.Dot
import Autolib.Hash
import Autolib.ToDoc


class ToTree baum where
      toTree :: baum -> Tree String

instance ToDot ( Tree String ) where
    toDotProgram t = Dot
    toDotOptions t = unwords [ "-Gordering=out", "-Gnodesep=0" ]
    toDot t = make t 

instance ( ToTree baum ) => ToDot baum  where
    toDotProgram = toDotProgram . toTree
    toDotOptions = toDotOptions . toTree
    toDot        = toDot        . toTree

instance Hash a => Hash ( Tree a ) where
    hash ( Node f args ) = hash ( f, hash args )


instance ( Eq baum, ToTree baum ) => Hash baum  where
    hash = hash . toTree


mirror :: Tree a -> Tree a
mirror ( Node f args ) = Node f ( map mirror args )

form :: ( ToTree baum ) 
     => baum -> Doc
form = vcat . map text . lines . drawTree . mirror 
     . fmap ( render . toDoc )
     . toTree



