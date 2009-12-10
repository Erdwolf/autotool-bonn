-- | TODO: move to SRS

module PCP.Reach where

import Autolib.Set
import Data.Ratio

type SRS c = [([c],[c])]

ex = [("0", "1112"), ("1", "2220"), ("2", "0001")]

-- | if it is possible to go from u to v,
-- then compute by what numbers of rule applications
numbers :: Ord c
	=> SRS c 
	-> ([c], [c])
	-> [ Rational ]
numbers srs (u, v) =
    let alpha = mkSet $ do (l,r) <- srs ; l ++ r
        table = do
	    (l, r) <- (u, v) : srs
            return $ do
		a <- setToList alpha
		return $ fromIntegral $ count a r - count a l 
    in  solve3 table
 

solve3 :: [[Integer]] -> [ Rational ]
solve3 [ y, x1, x2, x3 ] = 
    let d  = det3 [ x1, x2, x3 ]
        d1 = det3 [  y, x2, x3 ]
        d2 = det3 [ x1,  y, x3 ]
        d3 = det3 [ x1, x2,  y ]
    in  [ d1 % d, d2 % d, d3 % d ]

det3 :: Num a =>  [[a]] -> a
det3 [[a1,a2,a3],[b1,b2,b3],[c1,c2,c3]] 
    = a1 * ( b2 * c3 - b3 * c2 )
    - a2 * ( b1 * c3 - b3 * c1 )
    + a3 * ( b1 * c2 - b2 * c1 )

count :: Eq c => c -> [c] -> Int
count x xs = length $ filter (== x) xs