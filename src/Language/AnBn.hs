-- $Header$

module AnBn (anbn) where

-- $Log$
-- Revision 1.1  2002-11-08 16:43:23  joe
-- language -> Language
-- pump -> Pump.REG (neu)
-- aufgaben dazu
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.1  2001/12/05 21:11:31  autotool
-- lang/*
--
-- Revision 1.1  2001/12/04 13:04:10  autotool
-- neu: pump/* lang/*
--

import Language
import Set
import Monad ( guard )

anbn :: Language
anbn = Language 
       { abbreviation = "{ a^n b^n : n >= 0 }" 
       , alphabet     = mkSet "ab"
       , sample       = sam
       , contains     = con
       }

sam :: Int -> Int -> IO [ String ]
sam 0 n = return [ ]
sam c n = return $ do
    let (q, r) = n `divMod` 2
    guard $ 0 == r 
    return $ do x <- "ab"; replicate q x

con :: String -> Bool
con w = 
    let (q, r) = length w `divMod` 2
	(a, b) = splitAt q w
    in	0 == r && all (== 'a') a && all (== 'b') b 







