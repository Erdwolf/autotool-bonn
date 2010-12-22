module Baum.List.Ops where

import Baum.List.Type 
import Baum.Heap.Class
import Autolib.Size


instance Heap ListTree where

    -- empty :: baum a
    empty = Nil
    
    -- isEmpty :: baum a -> Bool 
    isEmpty t = case t of Nil -> True ; _ -> False

    -- insert :: Ord a => baum a -> a -> baum a
    insert t x = Cons x t

    -- deleteMin :: Ord a => baum a -> baum a
    deleteMin ( Cons x xs) = xs

    -- decreaseTo :: Ord a => baum a -> Position -> a -> baum a
    decreaseTo (Cons x xs) p y = case p of
        [] -> Cons y xs
        c : q -> Cons x ( decreaseTo xs q y )

    -- equal :: Eq a => baum a -> baum a -> Bool
    equal x y = True
    
    -- contents :: Ord a => baum a -> [a]
    contents t = case t of
        Nil -> []
        Cons x xs -> x : contents xs
