-- $Header$

module ABgleich ( abgleich ) where

-- $Log$
-- Revision 1.1  2002-11-08 16:43:23  joe
-- language -> Language
-- pump -> Pump.REG (neu)
-- aufgaben dazu
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.2  2002/01/14 23:11:14  autotool
-- anpassungen für serie 6
--
-- Revision 1.1  2001/12/05 21:11:31  autotool
-- lang/*
--

import Language
import Set
import Monad ( guard )
import Random

abgleich :: Language
abgleich = Language 
       { abbreviation = "{ w : |w|_a = |w|_b }"
       , alphabet     = mkSet "ab"
       , sample       = sam
       , contains     = con
       }

one 0 y = return $ replicate y 'b'
one x 0 = return $ replicate x 'a'
one x y = do
    f <- randomRIO (False, True)
    if f
       then do w <- one (x-1) y; return $ 'a' : w
       else do w <- one x (y-1); return $ 'b' : w

sam :: Int -> Int -> IO [ String ]
sam c n | odd n = return []
sam c n = sequence $ replicate c $ do
    let q = n `div` 2
    one q q

con :: String -> Bool
con w = 
    let count x = length ( filter (== x) w )
    in	count 'a' == count 'b'








