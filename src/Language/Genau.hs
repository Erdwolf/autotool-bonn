-- $Header$

module Genau where

-- $Log$
-- Revision 1.1  2002-11-08 16:43:23  joe
-- language -> Language
-- pump -> Pump.REG (neu)
-- aufgaben dazu
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.2  2001/12/12 08:13:36  autotool
-- fix: hat je einen buchstaben zuviel genommen
--
-- Revision 1.1  2001/12/10 00:37:33  autotool
-- einige neue sprachen
--

import Random

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
