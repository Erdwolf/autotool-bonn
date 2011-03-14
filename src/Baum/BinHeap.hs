{-# OPTIONS -fglasgow-exts #-}

module Baum.BinHeap

( make_fixed, make_quiz
)

where

import Baum.BinHeap.Type
import Baum.BinHeap.Ops
import Baum.BinHeap.Show

import qualified Baum.Heap.Class as C
import qualified Baum.Heap.Central

import qualified Tree as T
import Autolib.ToDoc
import Inter.Types
import Data.Typeable

instance Show a => T.ToTree ( BinHeap a ) where
    toTree = toTree . fmap show

data HeapbaumBinomial = HeapbaumBinomial
    deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Tag HeapbaumBinomial BinHeap Int where
    tag = HeapbaumBinomial

make_fixed :: Make
make_fixed = Baum.Heap.Central.make_fixed HeapbaumBinomial

make_quiz :: Make
make_quiz = Baum.Heap.Central.make_quiz HeapbaumBinomial

