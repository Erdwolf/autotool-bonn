{-# OPTIONS -fglasgow-exts #-}

module Baum.AVL 

( make_quiz
)

where

--  $Id$

import Baum.AVL.Type
import Baum.AVL.Ops
import Baum.AVL.Show

import qualified Baum.Such.Class as C
import qualified Baum.Such.Central

import qualified Tree as T
import Autolib.ToDoc
import Inter.Types
import Data.Typeable


instance C.Such AVLTree where
    empty = Leaf
    isEmpty = isLeaf

    contains = contains
    insert = insert
    delete = error "AVL.delete nicht implementiert"

    equal = (==)
    contents = contents

instance Show a => T.ToTree ( AVLTree a ) where
    toTree = toTree . fmap show

data SuchbaumAVL = SuchbaumAVL 
    deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Tag SuchbaumAVL AVLTree Int where
    tag = SuchbaumAVL

make_quiz :: Make
make_quiz = Baum.Such.Central.make_quiz SuchbaumAVL

