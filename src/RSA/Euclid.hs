module RSA.Euclid where

--  $Id$

-- | return (x, y) with ax + by = gcd a b
euclid :: Integral a
       => a -> a
       -> (a, a)
euclid a b =
    if b == 0 
    then ( 1 , 0 )
    else let (q, r) = divMod a b
	     (x, y) = euclid b r
	     -- gcd = b x + r y = b x + (a - q b) y = a y + b (x - q y)
	 in  (y, x - q * y)
