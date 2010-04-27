module Main (main) where

import Hilbert.Read

-- import Search
import Hilbert.Look

import System.IO

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
