{-# OPTIONS -fallow-overlapping-instances #-}

module LCS.Quiz where

--  $Id$

import LCS.Code
import LCS.Data
import LCS.Config

import Inter.Types

import Autolib.Set
import Autolib.Reader
import Autolib.ToDoc
import Random
import Autolib.Util.Wort
import Util.Datei
import Util.Cache
import Autolib.Util.Seed
import Autolib.Util.Zufall


roll :: ( InstanceC a )
     => Config a 
     -> IO ( [a], Instance a )
roll conf = do
     let sigma = setToList $ alphabet conf
     ( xs, ys, zs ) <- repeat_until
            ( do let l =  solution_length_min conf * length sigma 
		 sx <- randomRIO ( l `div` 2, l )
		 xs <- someIO sigma sx
		 sy <- randomRIO ( l `div` 2, l )
                 ys <- someIO sigma sy
                 let zs = lcs xs ys
		 return ( xs, ys, zs )
            ) ( \ (xs, ys, zs) ->
		    solution_length_min conf <= length zs
		 && length zs <= solution_length_max conf 
            ) 
     return ( zs
	    , Instance { left = xs, right = ys, sharp = exactly conf } 
	    )



