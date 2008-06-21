{-# language PatternSignatures #-}

import Robots3.Config
import Robots3.Quiz
import Robots3.Data
import Robots3.Nice
import Robots3.Generator

import Inter.Quiz

import Autolib.ToDoc

import Control.Monad ( when )

p :: RC
p = RC { width = 3
	, num_robots = 5
	, num_targets = 2
	, at_least = 5
	, search_width = 10000
	}

main = searcher 0

searcher k = do
    ( conf :: Config, sol :: [ Zug] ) <- generator Robots3 p undefined
    if ( length sol >= k ) 
        then do
            print $ nice conf
            print sol
        else do
            -- putStr "*"
            return ()
    searcher $ max k $ length sol


