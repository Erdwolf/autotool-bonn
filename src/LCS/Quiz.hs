module LCS.Quiz where

--  $Id$

import LCS.Code
import LCS.Data
import LCS.Instance

import Inter.Types

import Autolib.Set
import Random
import Autolib.Util.Wort
import Util.Datei
import Util.Cache
import Autolib.Util.Seed
import Autolib.Util.Zufall

data Config a =
     Config { alphabet :: Set a
	    , solution_length_min :: Int
	    , solution_length_max :: Int
	    , exactly :: Bool
	    }
     deriving Show

conf :: Config Char
conf = Config
	{ alphabet            = mkSet "abcde"
	, solution_length_min = 7
	, solution_length_max = 10
        , exactly = True
	}

huge :: Config Char
huge = Config
	{ alphabet            = mkSet "abc"
	, solution_length_min = 30
	, solution_length_max = 50
       , exactly = False
	}


roll :: InstanceC a
     => Config a 
     -> IO ( Instance a )
roll conf = do
     let sigma = setToList $ alphabet conf
     ( xs, ys, l ) <- repeat_until
            ( do let l =  solution_length_min conf * length sigma 
		 sx <- randomRIO ( l `div` 2, l )
		 xs <- someIO sigma sx
		 sy <- randomRIO ( l `div` 2, l )
                 ys <- someIO sigma sy
                 let zs = lcs xs ys
		 return ( xs, ys, length zs )
            ) ( \ (xs, ys, l) ->
		    solution_length_min conf <= l
		 && l <= solution_length_max conf 
            ) 
     return $ Instance { left = xs, right = ys, sharp = exactly conf }

quiz :: InstanceC a
     => Config a
     -> Var LCS (Instance a) [a]
quiz conf = 
    x where x = Var { problem = LCS
	  , tag = "LCS" ++ "-" ++  "Quiz"
	  , key = \ matrikel -> return matrikel
	  , gen = \ key -> do
		seed $ read key  
		i <- cache (  Datei { pfad = [ "autotool", "cache"
                                              , tag x
                                              ]
                                     , name = key
                                     , extension = "cache"
                                     }
                            ) ( roll conf )
		return $ return i
	  }
