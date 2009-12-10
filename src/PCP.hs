-- | standalone solver
-- command line arguments:
-- depth, width, instance

module Main where

import PCProblem.Family
import PCProblem.Param

import System
import IO

main = runit
      $ Param { alpha = "0123"
	      , paare = 4
	      , breite = 6
	      , nah = 0
	      , fern = 100
	      , viel = 10000
	      }
