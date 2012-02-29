module AVLBaumBonn.Conversion (toTree) where

import qualified Data.Tree as T

import Baum.AVL.Type (isLeaf, left, right, key, AVLTree)


toTree :: Baum.AVL.Type.AVLTree Int -> T.Tree (Maybe Int)
toTree t = T.unfoldTree uf t
   where
       uf t |       isLeaf t       = (Nothing,[])
            | isLeaf l && isLeaf r = (Just k,[])
            |             isLeaf r = (Just k,[l,r])
            | isLeaf l             = (Just k,[l,r])
            |       otherwise      = (Just k,[l,r])
        where k = key t
              l = left t
              r = right t
