{-# OPTIONS -fallow-overlapping-instances #-}

import PCP.Top
import System

main = do
    [ f ] <- getArgs
    mapM_ print $ find $ read f
