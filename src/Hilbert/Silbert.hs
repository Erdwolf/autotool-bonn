module Main (main) where

import Read

--import Search
import Look

import IO
import System

---------------------------------------------------------------

online cs =  
  sequence [ do putStrLn $ "search proof for: " ++ show x
		search x 
	   | x <- parsed cs
	   ]

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
