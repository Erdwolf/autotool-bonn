module Language.ABgleich ( abgleich ) where

-- -- $Id$

import Language.Type

import Data.Set
import Control.Monad ( guard )
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








