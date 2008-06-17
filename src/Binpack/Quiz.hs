module Binpack.Quiz where

import qualified Binpack.Instance as I
import qualified Binpack.Param as P
import Binpack.Approximation

import System.Random

pick :: P.Param -> IO [ Integer ]
pick c = do
    let bins = replicate ( P.bins c ) ( P.capacity c )
        fun bins = do
            let s = sum bins
            if s == 0 then return [] else do
                k <- randomRIO ( 1, s )
                let (w, bins') = select (2 * P.capacity c `div` 3) k bins
                ws <- fun bins'
                return $ w : ws
    fun bins

select top k (b : bs) = 
    if k <= b
    then let k' = if k > top then max 1 (2*top - k) else k
         in ( k', ( b - k' ) : bs )
    else let ( w, rest ) = select top ( k - b ) bs
         in  ( w, b : rest )
