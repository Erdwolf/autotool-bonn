module Search

( search
)

where

import Syntax

import Infer
import Tree
import Read

import System
import IO

mktree goal = mkt ([ goal ], "goal")

mkt (todo, inf) = 
    Node (todo, inf) [ mkt next | next <- infer todo ]

search :: Exp -> IO ()
search goal = 
  sequence
	[  do putStrLn $ "\nlayer " ++ show k
	      hFlush stdout
	      sequence [ if flag 
			 then do putStrLn "\n******** begin solution ********"
				 sequence [ putStrLn i
					  | (p, i) <- pinfs
					  ]
				 putStrLn "\n******** end solution **********"
			 else putStr "" -- putStr "."
		       | pinfs <- lpaths k t
		       , let flag = null (fst (last pinfs))
		       ]
	| let t =   mktree $ goal
	, k <- [ 1 .. ]
	]

[ goal0 ] = parsed "x -> x"
[ goal1 ] = parsed "not x -> (x -> y)"
[ goal2 ] = parsed "(x -> (y -> z)) -> (y -> (x -> z))"
[ goal3 ] = parsed "(x -> y) -> ((x -> (y -> z)) -> (x -> z))"

