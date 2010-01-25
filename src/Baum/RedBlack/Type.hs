{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell #-}

module Baum.RedBlack.Type where

import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable
     
data RedBlackColor = Red | Black
  deriving ( Eq, Typeable )

data RedBlackTree a = Empty
  | RedBlackTree RedBlackColor ( RedBlackTree a ) a ( RedBlackTree a )
  deriving ( Eq, Typeable )

$(derives [makeReader, makeToDoc] [''RedBlackTree])
$(derives [makeReader, makeToDoc] [''RedBlackColor])

instance Functor RedBlackTree where
  fmap f = foldt ( \ color left key right -> RedBlackTree color left ( f key ) right ) Empty

isLeaf :: RedBlackTree a -> Bool
isLeaf Empty = True
isLeaf _     = False

foldt redblacktree empty Empty = empty
foldt redblacktree empty ( RedBlackTree color left key right ) = redblacktree color ( foldt redblacktree empty left ) key ( foldt redblacktree empty right )
