-- -- $Id$

module Language.AnBnCn (anbncn) where

import Language.Type

import Set
import Monad ( guard )

anbncn :: Language
anbncn = Language 
       { abbreviation = "{ a^n b^n c^n : n > 0 }" 
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
    in	not ( null w ) 
	&& 0 == r && all (== 'a') a && all (== 'b') b && all (== 'c') c








