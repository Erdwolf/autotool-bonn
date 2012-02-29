{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module AVLBaumBonn.Conversion (AVLTreeBonn(..), bonnifyTree, debonnifyTree, toTree) where

import qualified Data.Tree as T
import qualified Baum.AVL.Type as A

import Data.Typeable (Typeable)
import Autolib.ToDoc
import Autolib.Reader


data AVLTreeBonn = Node Int AVLTreeBonn AVLTreeBonn
                 | Empty
  deriving (Typeable)

$(derives [makeReader, makeToDoc] [''AVLTreeBonn])

bonnifyTree :: A.AVLTree Int -> AVLTreeBonn
bonnifyTree t | A.isLeaf t = Empty
bonnifyTree t              = Node (A.key t) (bonnifyTree (A.left t)) (bonnifyTree (A.right t))

debonnifyTree :: AVLTreeBonn -> A.AVLTree Int
debonnifyTree (Node x l r) = A.branch (debonnifyTree l) x (debonnifyTree r)
debonnifyTree Empty        = A.leaf


toTree :: A.AVLTree Int -> T.Tree (Maybe Int)
toTree t = T.unfoldTree uf t
   where
       uf t |        A.isLeaf t        = (Nothing,[])
            | A.isLeaf l && A.isLeaf r = (Just k,[])
            |               A.isLeaf r = (Just k,[l,r])
            | A.isLeaf l               = (Just k,[l,r])
            |       otherwise          = (Just k,[l,r])
        where k = A.key t
              l = A.left t
              r = A.right t
