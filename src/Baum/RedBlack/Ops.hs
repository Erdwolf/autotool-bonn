module Baum.RedBlack.Ops where

--  $Id$

import Baum.RedBlack.Type 

contains :: Ord a => RedBlackTree a -> a -> Bool
contains Empty _ = False
contains (RedBlackTree _ left key right) k
  | k == key  = True
  | k <  key  = contains left  k
  | otherwise = contains right k

contents :: Ord a => RedBlackTree a -> [a]
contents = foldt ( \ color left key right -> left ++ [key] ++ right ) [] 

--------------------------------------------------------------------------

rbInsert :: Ord a => RedBlackTree a -> a -> RedBlackTree a
rbInsert t x = rbDyeNodeBlack ( rbInsert' x t)

-- Equal node values are inserted into the left subtree.
rbInsert' :: Ord a => a -> RedBlackTree a -> RedBlackTree a
rbInsert' x Empty = RedBlackTree Red Empty x Empty
rbInsert' x ( RedBlackTree color left y right )
            | x <= y = rbBalance color ( rbInsert' x left ) y right
            | x >  y = rbBalance color left y ( rbInsert' x right )

rbDyeNodeBlack :: Ord a => RedBlackTree a -> RedBlackTree a
rbDyeNodeBlack ( RedBlackTree _ left x right ) = RedBlackTree Black left x right

{- Black grandparents take responsibility for recoloring their
   red children and grandchildren. The resulting tree might have
   a red root node (--> call to ``rbDyeNodeBlack'' in ``rbInsert''). -}
rbBalance :: Ord a => RedBlackColor -> RedBlackTree a -> a -> RedBlackTree a -> RedBlackTree a
-- 1) Left case in [1, Fig. 1]:
rbBalance Black ( RedBlackTree Red ( RedBlackTree Red a x b ) y c ) z d
  = RedBlackTree Red ( RedBlackTree Black a x b ) y ( RedBlackTree Black c z d )
-- 2) Top case in [1, Fig. 1]:
rbBalance Black ( RedBlackTree Red a x ( RedBlackTree Red b y c ) ) z d
  = RedBlackTree Red ( RedBlackTree Black a x b ) y ( RedBlackTree Black c z d )
-- 3) Bottom case in [1, Fig. 1]:
rbBalance Black a x ( RedBlackTree Red ( RedBlackTree Red b y c ) z d )
  = RedBlackTree Red ( RedBlackTree Black a x b ) y ( RedBlackTree Black c z d )
-- 4) Right case in [1, Fig. 1]:
rbBalance Black a x ( RedBlackTree Red b y ( RedBlackTree Red c z d ) )
  = RedBlackTree Red ( RedBlackTree Black a x b ) y ( RedBlackTree Black c z d )
-- 5) Any other case (no balancing necessary):
rbBalance color left x right = RedBlackTree color left x right
-- Note that the right hand sides of cases 1--4 are the same.
