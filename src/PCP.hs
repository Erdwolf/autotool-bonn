{-# OPTIONS -fallow-overlapping-instances #-}

import PCP.Top

import System
import IO

main = do
    [ f ] <- getArgs
    mapM_ printf $ find $ read f

printf x = do print x ; hFlush stdout
