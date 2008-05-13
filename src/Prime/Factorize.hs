module Prime.Factorize where

-- | find prime factorization if all prime factors are below bound
-- warning: uses trial division
easy :: Integer -> Integer -> Maybe [ (Integer, Int) ]
easy bound x = easy_for ( 2 : [ 3, 5 .. bound ] ) x

easy_for tts x = do
    t : ts <- return tts
    if t * t > x then Just [ (x, 1) ]
       else 
           let ( q, r ) = divMod x t
           in  if  0 == r then do
                   fes <- easy_for tts q
                   return $ bump t fes
               else easy_for ts x

bump t ((s,e) : rest) | t == s = (s,e+1) : rest
bump  t rest = (t,1) : rest
