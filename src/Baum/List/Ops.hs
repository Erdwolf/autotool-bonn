module Baum.List.Ops where

import Baum.List.Type 
import Baum.Heap.Class
import Autolib.Size

toListHelper t a = case t of
  Nil -> []
  (Cons x xs) -> (a, x) : toListHelper xs (0 : a)
  
deleteAtPosition (Cons x xs) p = case p of
  [] -> xs
  c : q -> Cons x $ deleteAtPosition xs q

instance Heap ListTree where

    -- empty :: baum a
    empty = Nil
    
    -- isEmpty :: baum a -> Bool 
    isEmpty t = case t of Nil -> True ; _ -> False

    -- insert :: Ord a => baum a -> a -> baum a
    insert t a = case t of
      Nil -> Cons a t
      (Cons x xs) -> if (a < x)
                     then Cons a (Cons x xs)
                     else Cons x $ insert xs a

    -- deleteMin :: Ord a => baum a -> baum a
    deleteMin ( Cons x xs) = xs

    -- decreaseTo :: Ord a => baum a -> Position -> a -> baum a
    decreaseTo (Cons x xs) p y = insert (deleteAtPosition (Cons x xs) p) y

    -- equal :: Eq a => baum a -> baum a -> Bool
    equal a b = case a of
      Nil -> case b of
        Nil -> True
        _  -> False
      (Cons x xs) -> case b of
        Nil -> False
        (Cons y ys) -> if (x == y)
                       then equal xs ys
                       else False

    toList t = case t of
      Nil -> []
      (Cons x xs) -> toListHelper t []
    
    -- contents :: Ord a => baum a -> [a]
    contents t = case t of
        Nil -> []
        Cons x xs -> x : contents xs
