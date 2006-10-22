module Language.Zahl where

-- -- $Id$

import Random

zahl :: Int -> IO String
-- ein Integer dieser Länge
zahl 0 = return "0"
zahl l = do
     c <- randomRIO ( '1' , '9' )
     cs <- sequence $ replicate l $ randomRIO ( '0', '9' )
     return $ c : cs 
