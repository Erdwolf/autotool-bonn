module Baum.Roll where

--  $Id$

import Baum.Type
import Baum.Label

import ToDoc
import Random
import qualified Util.Zufall

-- | make "random" binary tree of given size (must be odd)
-- contains no variables
rollbin :: Int -> IO ( Term a () )
rollbin s | s <= 2 = return $ Node () []
rollbin s = do
    x <- randomRIO (1, s-2)
    let sl = if even x then pred x else x
    let sr = s - 1 - sl
    l <- rollbin sl
    r <- rollbin sr
    return $ Node () [ l, r ]

-- | make random binary tree with given labels (in any order)
rollab :: [ c ] -> IO ( Term a c )
rollab cs = do
    t  <- rollbin $ length cs
    ds <- Util.Zufall.permutation cs
    return $ label Pre t ds

-- | make random binary tree of given size with labels a, b, ..
roll :: Int -> IO Baum
roll s = rollab $ do 
     c <- take s [ 'a' .. 'z' ]
     return $ mkunary [ c ] -- naja, die arities stimmen nicht
			  -- und werden sowieso ignoriert (hoffentlich)

