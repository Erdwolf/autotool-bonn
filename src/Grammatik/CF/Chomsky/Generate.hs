module Grammatik.CF.Chomsky.Generate where

import Grammatik.CF.Chomsky.Type

import Autolib.FiniteMap

import Control.Monad ( guard )
import Data.List ( nub )

main :: Ord a
     => Chomsky a -> [[ String ]]
main ch = let Just ws = lookupFM ( creation ch ) ( start ch ) in ws

-- | compute terminal words derivable from symbol
-- collect words of equal length into groups:
-- lookupFM ( creation ch ) v !! k = list of all words derivable
-- from variable v of length k
creation :: Ord a
         => Chomsky a -> FiniteMap a [[String]]
creation ch = 
    let sigma = nub $ map fst $ rules ch
        fm = listToFM $ do
               v <- sigma
               let words k = nub $ 
                     if k <= 0 then [ "" | v == start ch && eps ch ]
                     else if k == 1 then do
                         ( v', Left c ) <- rules ch
                         guard $ v' == v
                         return [ c ]
                     else do
                         ( v', Right ( x, y ) ) <- rules ch
                         guard $ v' == v
                         l <- [ 1 .. k -1 ]
                         let r = k - l
                         let Just wls = lookupFM fm x
                             Just wrs = lookupFM fm y
                         wl <- wls !! l
                         wr <- wrs !! r
                         return $ wl ++ wr
               return ( v, map words [ 0 .. ] ) 
    in  fm
