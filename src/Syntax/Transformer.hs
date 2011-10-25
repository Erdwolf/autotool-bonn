{-# LANGUAGE NoMonomorphismRestriction, TupleSections #-}
module Syntax.Transformer where

import Syntax.Syntax
import Syntax.Generics

import Data.List
import Data.Generics
import Control.Monad.Writer
import Control.Monad.State
import Control.Arrow

transform :: Language -> Language
transform = removeLeftRecursion . removeForks . removeLoops


removeLoops lang =
    fst $ flip runState (map show [1..]) $ do
        (xs,ys) <- runWriterT (everywhereM (mkM removeLoop) lang)
        return (xs++ys)

removeLoop (Loop g) = do
    x <- pop
    tell [ (x, g)
         , (x, g `Chain` Symbol x)
         ]
    return (Symbol x)
removeLoop g = return g

removeForks [] = []
removeForks ((x, g):rest) =
    let new = map (x,) $ paths g in
    new ++ removeForks rest


paths (Fork g1 g2)  = paths g1 ++ paths g2
paths (Chain g1 g2) = [ Chain x y | x <- paths g1, y <- paths g2 ]
paths x             = [x]

pop = do
    (x:xs) <- get
    put xs
    return x

data Rule = Rule String [Item] deriving Show
data Item = T String | N String deriving Show

toRule :: (String, Graph) -> Rule
toRule (x,g) = Rule x $ map toItem $ filter (/=Empty) $ everything (++) ([] `mkQ` q) g
  where q (Chain _ _) = []
        q g           = [g]

toItem :: Graph -> Item
toItem (Symbol   x) = N x
toItem (Terminal x) = T x


fromRule :: Rule -> (String, Graph)
fromRule (Rule x g) = (x, foldr Chain Empty $ map fromItem g)

fromItem :: Item -> Graph
fromItem (N x) = Symbol x
fromItem (T x) = Terminal x

-- Paull's algorithm
--
removeLeftRecursion lang = do
    map fromRule $ f (zip [0..] xs) $ map toRule lang
  where
    xs = map fst lang -- Arbitrarily ordered list of all non-terminals

    f [] = id
    f ((i,x):ixs) = f ixs . removeDirect . g (take i xs)
      where
        g [] l = l
        g (y:ys) l = g ys $ h l
          where
            h [] = []
            h ((Rule x' (N y':a)):rs) | x == x' && y == y' =
                [ Rule x (b++a) | Rule y' b <- l, y == y' ] ++ h rs
            h (r:rs) = r : h rs

        removeDirect = concatMap h
          where
            h (Rule x' (N x'':_)) | x == x' && x' == x'' =
                []
            h r = [r]
