module Faktor.Certify where

--  $Id$

import Faktor.Prim
import Autolib.ToDoc

import Autolib.Util.Wort
import Autolib.Util.Zufall
import Autolib.Ana
import Control.Monad ( guard )
import Data.Maybe

next_certified_prime :: Integer -> ( Integer, Doc )
next_certified_prime from = head $ do
    n <- [ from .. ]
    guard $ and $ do
        p <- mediumish
	return $ 0 /= mod n p
    proof <- maybeToList $ certify n
    return ( n, proof )

-- | with given number of digits
some_certified_prime :: Int -> IO ( Integer, Doc )
some_certified_prime w = do
    x <- eins [1 .. 9 ]
    xs <- someIO [ 0 .. 9 ] $ pred w
    let n = unbased 10 $ x : xs
    return $ next_certified_prime n

-- | try to certify that n is prime
certify :: Integer
	-> Maybe Doc
certify n = do
    eul <- euler n
    proof <- listToMaybe 
	  $ concat
	  $ map (maybeToList . prim_root n) 
	  $ smallish
    return $ vcat
	   [ text "The number" <+> toDoc n <+> text "is prime, because"
	   , nest 4 $ proof
	   ]

-- | try to check non-primality
euler :: Integer -> Maybe Doc
euler n = do
    proof <- sequence $ do
        let m = pred n
        b <- smallish
	guard $ b < n
        return $ do
            let y = powmod b m n
            guard $ 1 == y
            return $ say b (text "n-1") y (text "n")
    return $ vcat
	   [ toDoc n <+> text "might be prime. because"
	   , nest 4 $ vcat proof
	   ]

-- | try to prove that b is primitive root for n
prim_root :: Integer -> Integer 
	  -> Maybe Doc
prim_root n b = do
    let m = pred n
    -- all factors small
    fks <- factor m
    proof <- sequence $ do
        (f, k) <- fks
	return $ do
	    let e = m `div` f
	    let r = powmod b e n
	    guard $ 1 < r
	    return (f, k, r)
    return $ vcat
	   [ toDoc b <+> text "is a primitive root for n =" <+> toDoc n 
	     <+> text "because"
	   , nest 4 $ vcat
		    [ text "the prime factorization of" <+> toDoc (pred n)
		                      <+> text "is" <+> toDoc fks
		    , nest 4 $ vcat $ do
			(f, k, r) <- proof
			return $ say b (text "(n - 1) /" <+> toDoc f) r (text "n")
		    ]
	   ]

say b e r n = hsep
		[ toDoc b , text "^" , parens e, text "=" , toDoc r 
		, text "mod", n
		]

-- | fast exponentiation modulo
-- TODO: should be done via an FFI call to gmp
powmod :: Integral b 
       => b -> b -> b -> b
powmod x 0 m = 1
powmod x n m = 
    let (q, r) = divMod n 2
        h = powmod x q m
	hh = (h * h) `mod` m
    in  if 0 == r 
	then hh
	else (hh * x) `mod` m
