module Look

( search
)

where

import Set
import Sorters

import Syntax

import Env
import Sub

import Infer
import Tree
import Read

import Sat

import System
import IO


maxdepth = 15	-- of tree
maxbranch = 5	-- of number of children of a tree node
maxwidth = 3	-- of clauses in search target

maxsize = 20	-- of formulas in target
maxvars = 4	-- of variables in target

maxsols = 10	-- number of solutions

search goal = search' 0 goal

search' dep goal = 
    do putStrLn $ "**** at depth " ++ show dep
       paths <- lookfor [ goal ] dep [ ] 
       if null paths
	  then search' (dep + 1) goal
	  else return ()

----------------------------------------------------------------       

lookfor :: [ Exp ] -> Int -> [ String ] -> IO [[String]]


lookfor []      depth path  =
    do	putStrLn $ "\n********** begin solution (length " ++ show (length path) ++ " *************"
	sequence [ print p
		 | p <- reverse path 
		 ]
	putStrLn $ "********** end solution   *************\n"

	return [ path ]


lookfor targets 0 path  = 
	return []

lookfor targets depth path  =
  do let s = sat targets
     if not s 
        then do -- putStrLn $ "--- NOT satisfiable: " ++ show targets
		return []
	else 
	  do 
  	    --putStrLn $ "+++ satisfiable: " ++ show targets

            if 2 * depth > maxdepth 
          	  then putStrLn $ show (depth, targets)
          	  else return ()
                    
            let ninfs = [ ni
          		   | ni @ (n, i) <- infer targets
          		   , length n <= maxwidth
          		   , maxsize > sum [  size t | t <- n ] 
          		   , maxvars > sum [ vsize t | t <- n ] 
			   , and [ not (isvar t) | t <- n ]
            		   ]
          
            let cands = take maxbranch
          		 $ msortwith ( \ (n,i) 
          			     -> sum [ size t  | t <- n ] )
          		 $ ninfs
          
            ps <- mapM ( \ (next, inf) ->
			 lookfor next (depth - 1) (inf : path) 
		       ) ninfs
	    return $ concat ps
          
          
size :: Exp -> Int
size (App fun args) = 1 + sum [ size arg | arg <- args ]


vsize = cardinality . filterSet isvar . subs 
