-- -*- mode: haskell -*-

import Challenger
import PCProblem.PCProblem

student = Aufgabe { problem = PCProblem
		  , instanz = bsp_pcp
		  , beweis = bsp_folge
		  }


bsp_pcp = PCP [("001","0"), ("10","011"), ("01","001") ]

bsp_folge =  [1,2,3]
