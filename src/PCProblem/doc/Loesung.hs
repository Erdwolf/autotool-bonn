-- -*- mode: haskell -*-

import Challenger
import PCProblem.PCProblem

student = Aufgabe { problem = PCProblem
		  , ident = Ident { aufgabe = 1 }
		  , beweis = bsp_folge
		  }

bsp_folge =  [1,2,3]
