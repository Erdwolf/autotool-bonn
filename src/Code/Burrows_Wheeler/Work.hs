module Code.Burrows_Wheeler.Work where

--  $Id$

import Autolib.Util.Sort

bw :: Ord a => [a] -> ( [a], Int )
bw xs =
    let fbw = fullbw xs
        it  = length $ takeWhile ( (/= 0) . snd ) $ fbw
    in  ( map ( last . fst ) fbw , it )

fullbw :: Ord a => [a] -> [ ([a], Int) ]
fullbw xs = sort $ do
    k <- [ 0 .. length xs - 1 ]
    return ( rotate k xs , k )

rotate k xs =
    let ( pre, post ) = splitAt k xs
    in  post ++ pre

