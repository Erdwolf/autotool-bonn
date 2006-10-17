module Hilbert.Tree

( Tree (..)
, layers
, paths
, lpaths
)

where

------------------------------------------------------------------------

data Tree a = Node a [Tree a]
  deriving (Eq, Ord, Show)

instance Functor Tree where
    map f = tfold ( \ x cs -> Node (f x) cs )

tfold :: (a -> [b] -> b) -> (Tree a -> b)
tfold f (Node x cs) = f x (map (tfold f) cs)


size :: Tree a -> Int
size = tfold ( \ _ cs -> sum (1 : cs) )


-------------------------------------------------------------------------

layers :: Tree a -> [[a]]
layers t = [ layer k t | k <- [0 .. ]]

layer :: Int -> Tree a -> [a]
layer 0 (Node x ys) = [x]
layer n (Node x ys) = concat [ layer (n-1) y | y <- ys ]

paths :: Tree a -> [[a]]
paths t = [ p | k <- [0..], p <- lpaths k t ]

lpaths 0 (Node x ys) = [ [x] ]
lpaths n (Node x ys) = [ x : p | y <- ys, p <- lpaths (n-1) y ]
