module EditDistance.CalculateTable where

table :: String -> String -> [[Int]]
table s t = dt where
  n = length s
  m = length t
  d (i,j) | i==n         = m-j
          | j==m         = n-i
          | s!!i == t!!j = min3 (1 + dt !! i !! (j+1),
                                 1 + dt !! (i+1) !! j,
                                 dt !! (i+1) !! (j+1))
          | otherwise    = 1 + min3 (dt !! i !! (j+1),
                                     dt !! (i+1) !! j,
                                     dt !! (i+1) !! (j+1))
  dt = [ [ d (i,j) | j <- [0..m] ] | i <- [0..n] ]
  min3 (x,y,z) = min x (min y z)
