module Prime.Check where

import Prime.Factorize

import Control.Monad ( guard )

-- | next larger (verifiable) prime number, with primitive root 
-- reasonable upper bound for applications: 10^14 (then runtime is a few seconds)
next :: Integer -> ( Integer, Integer )
next from = head $ do 
    p <- filter odd [ from .. ]
    Just g <- return $ primitive_root p
    return ( p, g )

-- | can be used for primality testing:
-- if it returns a number, then argument is a verified prime.
-- it may miss some primes because of built-in cut-offs.
primitive_root :: Integer -> Maybe Integer
primitive_root p = do
    guard $ fermat_tests p
    fes <- Prime.Factorize.easy factorize_bound ( p - 1 )
    find_root p ( map fst fes ) [ 2 .. root_bound ]

-- | FIXME arbitrary number
factorize_bound :: Integer
factorize_bound = 10^6 

-- | FIXME arbitrary number
root_bound :: Integer
root_bound = 100

fermat_test :: Integer -> Integer -> Bool
fermat_test p g = 1 == powMod g (p-1) p

fermat_tests :: Integer -> Bool
fermat_tests p = and $ do
    g <- [ 2 .. min (p-1) root_bound ]
    return $ fermat_test p g


find_root p fs ggs = do
    g : gs <- return ggs
    if root_test p g fs
       then return g 
       else find_root p fs gs

is_primitive_root p g = fermat_test p g &&
    case Prime.Factorize.easy factorize_bound ( p - 1 ) of
         Nothing -> False
         Just fes -> root_test p g $ map fst fes

root_test p g fs = and $ do
          f <- fs
          let ( e, 0 ) = divMod ( p - 1 ) f
          return $ 1 /= powMod g e p

powMod b 0 m = 1
powMod b e m = 
    let ( q, r ) = divMod e 2
        b2 = ( ( powMod b q m ) ^ 2 ) `mod` m
    in  case r of
            0 -> b2
            1 -> ( b2 * b ) `mod` m

