-- TODO: should go into lib/

module PCP.DFS where

import Data.Set

dfs :: Ord a => (a -> Set a) -> a -> [a]
dfs next x0 = 
    let dfs' (x : todo) done = x : 
           let now = union done $ unitSet x
               neu = minusSet (next x) now
           in  dfs' (setToList neu ++ todo) now
        dfs' [] done = []
    in  dfs' [x0] emptySet
