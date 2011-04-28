{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses #-}

module Baum.Leftist

( make_fixed, make_quiz
)

where

--  $Id$

import Baum.Leftist.Type
import Baum.Leftist.Ops
import Baum.Leftist.Show

import qualified Baum.Heap.Class as C
import qualified Baum.Heap.Central

import qualified Tree as T
import Autolib.ToDoc
import Inter.Types
import Data.Typeable


instance Show a => T.ToTree ( LeftistTree a ) where
    toTree = toTree . fmap show

data HeapbaumLeftist = HeapbaumLeftist 
    deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Tag HeapbaumLeftist LeftistTree Int where
    tag = HeapbaumLeftist

make_fixed :: Make
make_fixed = Baum.Heap.Central.make_fixed HeapbaumLeftist

make_quiz :: Make
make_quiz = Baum.Heap.Central.make_quiz HeapbaumLeftist


