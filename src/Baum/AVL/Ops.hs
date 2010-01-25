module Baum.AVL.Ops where

--  $Id$

import Baum.AVL.Type 

contains :: Ord a => AVLTree a -> a -> Bool
contains Leaf _ = False
contains t k
  | k == key t = True
  | k < key t = contains (left t) k
  | otherwise = contains (right t) k

insert :: Ord a => AVLTree a -> a -> AVLTree a
insert t v = (\(new_t, g) -> new_t) $ insert_avl t v

-- delete :: Ord a => AVLTree a -> a -> AVLTree a

--equal :: Eq a => AVLTree a -> AVLTree a -> Bool
--equal = (==)

contents :: Ord a => AVLTree a -> [a]
contents = foldt (\l k w r -> l ++ [k] ++ r) [] 


--------------------------------------------------------------------------

insert_avl :: (Ord a) => AVLTree a -> a -> (AVLTree a, Bool)
insert_avl Leaf v = (Branch Leaf v 0 Leaf, True)
insert_avl (Branch l k w r) v 
  | v <= k = let (t, g) = insert_avl l v
             in  if (g) 
                 then grown_left $ Branch t k w r 
                 else (Branch t k w r, g)
                           
  | v >  k = let (t, g) = insert_avl r v
             in   if (g) 
                  then grown_right $ Branch l k w t 
                  else (Branch l k w t, g)

rotation_right :: AVLTree a -> AVLTree a   
rotation_right (Branch l k w r) = Branch (left l)
                                         (key l)
                                         w
                                         (Branch (right l) k w r)
                                       
rotation_left :: AVLTree a -> AVLTree a                                 
rotation_left (Branch l k w r) = Branch (Branch l k w (left r))
                                        (key r)
                                        w
                                        (right r)

grown_left :: AVLTree a -> (AVLTree a, Bool)
grown_left t@(Branch l k w r)
        | w == (-1) = if (weight l == -1)
                      then ((\(Branch l k w r) -> Branch l k w (modBalance r 0)) ( rotation_right (modBalance t 0) ), False)
                      else (new_Balance_left (rotation_right (Branch (rotation_left l) k 0 r)), False)
        | w == 0  = (Branch l k (-1) r , True)
        | w == 1  = (Branch l k 0 r , False)

grown_right :: AVLTree a -> (AVLTree a, Bool)
grown_right t@(Branch l k w r)
        | w == 1 = if (weight r == 1)
                   then ((\(Branch l k w r) -> Branch (modBalance l 0) k w r) ( rotation_left (modBalance t 0) ), False)
                   else (new_Balance_right (rotation_left (Branch l k 0 (rotation_right r))), False)
        | w == 0  = (Branch l k 1 r , True)
        | w == (-1)  = (Branch l k 0 r , False)


new_Balance_left :: AVLTree a -> AVLTree a 
new_Balance_left (Branch l k w r) 
        | w == 1    = Branch (modBalance l (-1)) k w (modBalance r 0)
        | w == (-1) = Branch (modBalance l 1) k w (modBalance r 0)
        | w == 0    = Branch (modBalance l 0) k w (modBalance r 0)

-- | Balance Kontrollieren
new_Balance_right :: AVLTree a -> AVLTree a 
new_Balance_right (Branch l k w r) 
        | w == (-1) = Branch (modBalance l 0) k w (modBalance r 1)
        | w == 1    = Branch (modBalance l (-1)) k w (modBalance r 0)
        | w == 0    = Branch (modBalance l 0) k w (modBalance r 0)     

modBalance :: AVLTree a -> Int -> AVLTree a
modBalance (Branch l k _ r) w = Branch l k w r