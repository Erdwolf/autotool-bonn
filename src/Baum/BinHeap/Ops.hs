module Baum.BinHeap.Ops where

import Baum.BinHeap.Type
import Baum.Heap.Op ( Position )
import qualified Baum.Heap.Class  as C

import Control.Monad ( guard )
import Data.List ( inits, tails )

instance C.Heap BinHeap where
    empty = BinHeap []
    isEmpty h = null $ roots h
    insert h x = BinHeap 
           $ merge ( roots h ) 
           [ Node { key = x, children = [] } ]
    deleteMin h | length ( roots h ) > 0 = head $ do
        ( pre, this : post ) <- splits $ roots h
        guard $ isMin this h
        return $ BinHeap 
               $ merge ( pre ++ post ) ( children this )
    get h p = gets ( roots h ) p
    decreaseTo h p x = 
        BinHeap $ decreaseTo ( roots h ) p x
    equal = (==)
    toList = toll . roots 

-- | merge order-increasing lists of trees
merge :: Ord a 
      => [BinTree a] -> [ BinTree a ] -> [BinTree a]
merge [] ys = ys      
merge xs [] = xs
merge (x:xs) (y:ys) = case compare ( order x ) ( order y ) of
    LT -> x : merge xs (y:ys)
    GT -> y : merge (x:xs) ys
    EQ -> merge [ glue x y ] $ merge xs ys
      
-- | make one tree from two trees of equal order          
glue :: Ord a 
      => BinTree a -> BinTree a -> BinTree a
glue x y | order x == order y =       
    if ( key x < key y ) 
    then x { children = children x ++ [y] }   
    else y { children = children y ++ [x] }         
      
toll :: [ BinTree a ] -> [(Position, a )] 
toll ts = do 
    (k,t) <- zip [ index_starts_at .. ] ts
    ( [k], key t ) : do 
        (p,x) <- toll $ children t
        return ( k : p, x )
    
isMin :: Ord a => BinTree a -> BinHeap a -> Bool
isMin t h = and $ map ( \ u -> key t <= key u ) $ roots h

index_starts_at = 0

gets :: [ BinTree a ] -> Position -> Maybe a
gets ts ps = case ps of 
    [] -> Nothing
    p : ps -> do
        let q = p - index_starts_at 
        guard $ 0 <= q && q < length ts
        let t = ts !! p
        if null ps then return $ key t else gets ( children t ) ps

decreaseTo :: Ord a  
           => [ BinTree a ] -> Position -> a -> [ BinTree a ]
decreaseTo ts (p : ps) x = 
    let ( pre, t : post ) = splitAt (p - index_starts_at) ts
        t' = if null ps 
             then Node { key = x , children = children t }
             else  if x < key t 
             then Node 
                  { key = x
                  , children = decreaseTo ( children t ) ps ( key t )
                  }             
             else Node 
                  { key = key t
                  , children = decreaseTo ( children t ) ps x
                  }             
    in  ( pre ++ t' : post ) 
           

splits xs = zip ( inits xs ) ( tails xs )
  
