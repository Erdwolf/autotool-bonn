{-# OPTIONS -fallow-overlapping-instances #-}

-- | standalone solver
-- command line arguments:
-- depth, width, instance

import PCProblem.Family
import PCProblem.Param

import System
import IO

main = runit
      $ Param { alpha = "01"
	      , paare = 4
	      , breite = 4
	      , nah = 1
	      , fern = 100
	      , viel = 3000
	      }
