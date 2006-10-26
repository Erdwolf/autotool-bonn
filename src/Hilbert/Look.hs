module Hilbert.Look

( search
)

where

import Autolib.Set
import Autolib.Util.Sort
import Autolib.Util.Hide
import Autolib.FiniteMap
import Autolib.Util.Uniq

import Hilbert.Infer
import Hilbert.Sat

import Autolib.TES.Term hiding ( unvar, assoc, precedence, arity )
import Autolib.Size
import Autolib.Schichten
import Autolib.TES.Position
import Boolean.Op
import Expression.Op

import Control.Monad ( guard, when )
import System.IO

maxdepth = 15	-- of tree
maxbranch = 500	-- of number of children of a tree node
maxwidth = 4	-- of clauses in search target

maxsize = 20	-- of formulas in target
maxvars = 4	-- of variables in target

maxsols = 10	-- number of solutions


search :: Exp Bool -> IO ()
search goal = search' 0 $ unvar goal

search' dep goal = 
    do putStrLn $ "**** at depth " ++ show dep
       paths <- lookfor_bfs [ goal ] dep [ ] 
       if null paths
	  then search' (dep + 1) goal
	  else return ()

----------------------------------------------------------------       

lookfor_bfs targets top path = do
  sequence_ $ do
    ( n, Hide inf ) <- bfs ( \ ( ts, Hide p ) ->        mkSet $ do
        guard $ satisfiable ts 
	( n, i ) <- infer ts
	guard $ klein top n
	return ( n , Hide ( p ++ "\n" ++ i ) )
      ) ( targets, Hide path ) 
    return $ do 
        -- print ( n, inf ) 
	putStr $ show $ length n ; hFlush stdout
	when ( null n ) $ error $ inf
  return []

klein top ts = and
      [ length ts < maxwidth
      , sum ( map size ts ) < top
      , cardinality ( unionManySets $ map vars ts ) < maxvars 
      ]


lookfor :: [ Exp Bool ] -- ^  Konjunktion, soll erfüllt werden
	-> Int          -- ^  Suchtiefe
	-> [ String ]   -- ^ Suchpfad ( prefix )
	-> IO [[String]] -- ^  alle (manche?) Lösungen

lookfor []      depth path  =
    do	putStrLn $ "\n********** begin solution (length " ++ show (length path) ++ " *************"
	sequence [ print p
		 | p <- reverse path 
		 ]
	error $ "********** end solution   *************\n"

	return [ path ]


lookfor targets 0 path  = 
	return []

lookfor targets depth path  =
  do let s = satisfiable targets
     if not s 
        then do putStrLn $ "--- NOT satisfiable: " ++ show targets
		return []
	else do 
  	    putStrLn $ "+++ targets: " ++ show targets

            if 2 * depth > maxdepth 
          	  then putStrLn $ show (depth, targets)
          	  else return ()
                    
            let ninfs = [ ni
          		   | ni @ (n, i) <- infer targets
          		   , length n <= maxwidth
          		   , maxsize > sum [  size t | t <- n ] 
          		   , all ( \ t -> maxvars >= variables t ) n
			   , and [ not (isvar t) | t <- n ]
            		   ]
          
            let cands = take maxbranch
          		 $ sortBy ( \ (n,i) 
          			     -> ( 0 -- negate $ length n
					, sum [ (size t)^ 2  | t <- n ] ) 
					)
          		 $ ninfs
          
            ps <- mapM ( \ (next, inf) ->
			 lookfor next (depth - 1) (inf : path) 
		       ) ninfs
	    return $ concat ps
          
          
variables = length . voccs

