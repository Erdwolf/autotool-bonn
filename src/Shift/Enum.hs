module Shift.Enum where

subsets :: Int -> [a] -> [[a]]
subsets k xs | k < 0 = []
subsets 0 xs = [[]]
subsets k (x : xs) 
    =  subsets k xs
    ++ do ys <- subsets (k-1) xs ; return $ x : ys
