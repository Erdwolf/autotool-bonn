-- -- $Id$

module Language.AnBn ( top ) where

import Language
import Autolib.Set
import Control.Monad ( guard )

top :: Language
top = Language 
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







