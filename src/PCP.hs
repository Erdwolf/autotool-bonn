{-# OPTIONS -fallow-overlapping-instances #-}

import PCP.Compressed

import Autolib.ToDoc

import System
import IO

main = runit

{- 
  do
    ws <- getArgs
    mapM_ (printf. toDoc) 
        $ take 1
	$ topdown
	$ map read ws
-}
