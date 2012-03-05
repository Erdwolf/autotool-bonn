{-# LANGUAGE DeriveDataTypeable #-}
module HeapSort.Tree (Tree(..), branch, leaf, empty, showTree, fromList, toList) where

import Data.Ratio
import Data.Generics (Data, Typeable)



data Tree a = Branch a (Tree a) (Tree a)
            | Empty
    deriving (Data, Typeable, Functor)

branch = Branch
leaf x = Branch x Empty Empty
empty  = Empty

showTree Empty = "@"
showTree (Branch x Empty Empty) = show x
showTree (Branch x l r) = "(" ++ show x ++ " " ++ showTree l ++ " " ++ showTree r ++ ")"

fromList = bfb
toList   = bft


bfb xs = build []
  where
    n = length xs

    build ds =
        let idx = calc 0 ds
        in if idx >= n
            then Empty
            else Branch (xs !! idx) (build (0:ds)) (build (1:ds))

    calc i []     = 2^i - 1
    calc i (d:ds) = 2^i*d + calc (succ i) ds


bft t = concatMap g $ concat $ takeWhile (not . null) $ iterate (>>= f) [t]
  where
    f Empty = []
    f (Branch _ l r) = [l,r]
    g Empty = []
    g (Branch x _ _) = [x]


depth Empty = 0
depth (Branch _ l r) = 1 + max (depth l) (depth r)
