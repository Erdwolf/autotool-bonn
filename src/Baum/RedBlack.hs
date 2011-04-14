{-# LANGUAGE DeriveDataTypeable #-}

module Baum.RedBlack

( make_quiz )

where

--  $Id$

import Baum.RedBlack.Type
import Baum.RedBlack.Ops
import Baum.RedBlack.Show

import qualified Baum.Such.Class as C
import qualified Baum.Such.Central

import qualified Tree as T
import Autolib.ToDoc
import Inter.Types
import Data.Typeable

instance C.Such RedBlackTree where
  empty = Empty
  isEmpty = isLeaf
  contains = contains
  insert = rbInsert
  delete = error "Delete für RedBlackTree nicht implementiert"
  equal = (==)
  contents = contents

instance Show a => T.ToTree ( RedBlackTree a ) where
  toTree     = toTree     . fmap show
  toTreeNode = toTreeNode . fmap show

data SuchbaumRedBlack = SuchbaumRedBlack
  deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Tag SuchbaumRedBlack RedBlackTree Int where
  tag = SuchbaumRedBlack

make_quiz :: Make
make_quiz = Baum.Such.Central.make_quiz SuchbaumRedBlack

