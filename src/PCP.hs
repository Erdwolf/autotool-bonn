{-# OPTIONS -fallow-overlapping-instances #-}

import PCP.Compressed

import Autolib.ToDoc

import System
import IO

main = do

    ws <- getArgs
    mapM_ (printf. toDoc) 
        $ take 1
	$ topdown
	$ map read ws

printf x = do print x ; hFlush stdout

