{-# OPTIONS -fallow-overlapping-instances #-}

module PCP.Solve where

import Autolib.Schichten
import Autolib.Util.Hide
import Autolib.Sets

import Control.Monad
import Data.Maybe

import PCP.Type
import PCP.Examples

type Conf c = ( Maybe [c], Hide [Int] )

tree :: Ord c 
     => PCP c
     -> Int
     -> [([c], [Int])]
tree pcp width = do
    let next ( mdiff, Hide is ) = mkSet $ do
            let diff = fromMaybe [] mdiff

            guard $ length diff < width

            (i, (l, r)) <- zip [0..] pcp
            let (pre, post) = splitAt (length r) (diff ++ l)
            guard $ pre == r
	    return ( Just $ post, Hide $ i : is )
    ( Just d , Hide w ) <- bfs next ( Nothing, Hide [] )
    return ( d, reverse w )

around :: PCP c -> PCP c
around pcp = do
    (l, r) <- pcp
    return (reverse r, reverse l)

--------------------------------------------------------------------------

data Repeater c = This c
		| Repeat Int [ Repeater c ]


