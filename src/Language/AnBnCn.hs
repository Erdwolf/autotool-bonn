-- $Header$

module AnBnCn (anbncn) where

-- $Log$
-- Revision 1.1  2002-11-08 16:43:23  joe
-- language -> Language
-- pump -> Pump.REG (neu)
-- aufgaben dazu
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.1  2001/12/04 13:04:10  autotool
-- neu: pump/* lang/*
--

import Language
import Set
import Monad ( guard )

anbncn :: Language
anbncn = Language 
       { abbreviation = "{ a^n b^n c^n : n >= 0 }" 
       , alphabet     = mkSet "abc"
       , sample       = sam
       , contains     = con
       }

sam :: Int -> Int -> IO [ String ]
sam 0 n = return [ ]
sam c n = return $ do
    let (q, r) = n `divMod` 3
    guard $ 0 == r 
    return $ do x <- "abc"; replicate q x

con :: String -> Bool
con w = 
    let (q, r) = length w `divMod` 3
	(a, bc) = splitAt q w
	(b, c) = splitAt q bc
    in	0 == r && all (== 'a') a && all (== 'b') b && all (== 'c') c








