module Language.Genau where

-- -- $Id$

import System.Random

genau :: [ (Char, Int) ] -> IO String
-- genau [(c1,n1),..] würfelt einen String
-- mit genau n1 mal buchstabe c1 usw.
genau [] = return []
genau xns = do
    let l = length xns
    i <- randomRIO (0, l-1)
    let (pre, (x,n) : post) = splitAt i xns
    let xns' = pre ++ [ (x, n-1) | n > 1 ] ++ post
    w <- genau xns'
    return $ x : w
