{-# LANGUAGE DeriveDataTypeable #-}

module Baum.List

( make_fixed, make_quiz
)

where

--  $Id$

import Baum.List.Type
import Baum.List.Ops
import Baum.List.Show

import qualified Baum.Heap.Class as C
import qualified Baum.Heap.Central

import qualified Tree as T
import Autolib.ToDoc
import Inter.Types
import Data.Typeable


instance Show a => T.ToTree ( ListTree a ) where
    toTree = toTree . fmap show

data HeapbaumList = HeapbaumList 
    deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Tag HeapbaumList ListTree Int where
    tag = HeapbaumList

make_fixed :: Make
make_fixed = Baum.Heap.Central.make_fixed HeapbaumList

make_quiz :: Make
make_quiz = Baum.Heap.Central.make_quiz HeapbaumList


