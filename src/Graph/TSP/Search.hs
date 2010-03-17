module Graph.TSP.Search where

import Graph.TSP.Tropic

import Autolib.ToDoc
import Autolib.Util.Sort ( sortBy )

import Data.Array
import Control.Monad ( guard )

data State w = 
     State { dist :: Array (Int,Int) (Tropic w)
           , path :: [ Int ] -- ^ finally, path has length (n+1) because head == last
           , weight :: Tropic w -- ^ of the edges 
           , todo :: Int -- ^ number of edges that we need
           , has_output :: Array Int Bool
           , has_input :: Array Int Bool 
           }
    deriving Show

search ::  ( ToDoc w, Num w, Ord w ) 
      => [[Tropic w]] 
      -> [ State w ]
search m = bb ( start m ) Infinite ( \ b -> [] )

-- | return strictly decreasing list of solutions below bound.
-- precondition: bound s < b.
-- the continuation is called with the current bound
-- (the weight of the most recent solution)
bb s b cont = 
    if 0 == todo s 
    then s : cont ( weight s )
    else bb_list ( forwards s ) b cont

bb_list [] b cont = cont b
bb_list (s : ss) b cont = 
    if bound s < b 
    then bb s b $ \ b' -> bb_list ss b' cont 
    else bb_list ss b cont


-- | all possible one-step extensions
forwards :: ( ToDoc w, Num w ) 
        => State w 
        -> [ State w ]
forwards s = do
    let x = last $ path s
    (y, False) <- assocs $ has_input s
    guard $ x /= y
    guard $ 1 == todo s || not ( has_output s ! y ) 
    guard $ dist s ! (x,y) /= Infinite
    return $ forward s y


-- | extend path 
forward :: ( ToDoc w, Num w ) 
        => State w -> Int -> State w
forward s y = 
    let x = last $ path s
    in  if has_output s ! x then error $ "forward: x already has output" ++ show (s,y)
        else if has_input s ! y then error $ "forward: y already has input" ++ show (s,y)
        else s { path = path s ++ [ y ]
               , weight = weight s + dist s ! (x,y)
               , todo = pred $ todo s
               , has_output = has_output s // [ (x,True) ] 
               , has_input = has_input s // [ (y,True) ] 
               }


-- | initial configuration, start path at index 0
start :: ( ToDoc w, Num w ) 
      => [[Tropic w]] -> State w
start m = 
    let n = length m
    in  State
        { dist = listArray ((0,0),(n-1,n-1)) $ concat m
        , path = [0], todo = n , weight = 0
        , has_input  = accumArray undefined False (0,n-1) []
        , has_output = accumArray undefined False (0,n-1) []
        }

-- | compute lower bound on the weight of any solution
-- that extends the path of this solution.
bound :: ( Num w, Ord w, ToDoc w ) 
      => State w -> Tropic w
bound s = weight s + max ( outbound s ) ( inbound s )

outbound :: ( Num w, Ord w, ToDoc w ) 
         => State w -> Tropic w
outbound s = sum $ do 
        (x,False) <- assocs $ has_output s
        return $ minimum $ Infinite : do
            (y,False) <- assocs $ has_input s
            return $ dist s ! (x,y)

inbound :: ( Num w, Ord w, ToDoc w ) 
        => State w -> Tropic w
inbound s = sum $ do 
        (x,False) <- assocs $ has_input s
        return $ minimum $ Infinite : do
            (y,False) <- assocs $ has_output s
            return $ dist s ! (x,y)

