module Baum.List.Ops where

import Baum.List.Type 
import Autolib.Size


contains :: Ord a => ListTree a -> a -> Bool
contains t k = foldt 
    ( \ l j r -> case compare k j of
           LT -> l ; EQ -> True ; GT -> r ) 
    False
    t

-----------------------------------------------------

repeated_insert_ok :: [ Int ] -> Bool
repeated_insert_ok xs = 
    proper $ foldl insert leaf xs 
    -- size ( foldl insert leaf xs ) == length xs
    

-----------------------------------------------------

rotate_right t = 
    branch ( left $ left t ) 
           ( key $ left t )
           ( branch ( right $ left t )
                    ( key t )
                    ( right t ) )

rotate_left t = 
    branch ( branch ( left t )
                    ( key t )
                    ( left $ right t ) )
           ( key $ right t )
           ( right $ right t ) 

rotate_left_then_right t = 
    rotate_right $ branch 
                 ( rotate_left $ left t )
                 ( key t )
                 ( right t )

rotate_right_then_left t =
    rotate_left $ branch
                ( left t )
                ( key t )
                ( rotate_right $ right t )
           
rebalance b = 
    case (weight $ left b, weight b, weight $ right b) of
        ( _, w, _ ) | abs w < 2 -> b
        ( -1, -2, _)  -> rotate_right b
        ( 1, -2, _) -> rotate_left_then_right b
        ( _, 2, 1) -> rotate_left b
        ( _, 2, -1) -> rotate_right_then_left b


insert t k = 
    if isLeaf t 
    then branch leaf k leaf
    else rebalance $ if k < key t 
         then branch ( insert ( left t ) k ) (key t)(right t)
         else branch (left t)( key t)(insert (right t) k)

