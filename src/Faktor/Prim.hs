module Faktor.Prim where

--  $Id$

smallish :: [ Integer ]
smallish = primes 10

mediumish :: [ Integer ]
mediumish = primes 100


-- | list of prime numbers up to q*q
primes :: Integral b 
       => b -> [ b ]
primes q = 2 : sieve [ 3, 5 .. q * q - 1 ] where
    sieve (x : ys) = 
	x : if x > q 
	    then ys
            else sieve ( filter ( \ y -> 0 /= mod y x ) ys )

-- | by trial division
-- but: return Nothing if no factorisation was found

factor :: Integer -> Maybe [ (Integer, Int) ]
factor x = trial mediumish x

trial :: [ Integer ] 
      -> Integer 
      -> Maybe [ (Integer, Int) ]
trial [] x = Nothing
trial (p : ps) x =
    if x == 1
    then return []
    else if p * p > x 
    then return [(x, 1)] -- is prime
    else do
	 let (k, q) = divvy x p
	 rest <- trial ps q
	 return $ [ (p, k) | k > 0 ] ++ rest

divvy :: Integral b
      => b -> b -> (Int, b)
divvy x p =
    let (q, r) = divMod x p
	(k', q') = divvy q p
    in  if 0 < r then (0, x)
	else (succ k', q')


