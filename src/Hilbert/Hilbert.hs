module Main (main) where

import Actions

import IO
import System

---------------------------------------------------------------

online = acts . lines

mainf argv =
     do
        h <- case argv of
                  [] -> return stdin
                  ["-"] -> return stdin
                  [f] -> openFile f ReadMode 
                  _ -> error "need one arg: input file name"
        cs <- hGetContents h
        env <- online $ cs
	return ()


main = 
    do
        argv <- getArgs
        mainf argv
