-- -*- mode: haskell -*-

import Challenger
import Col.DreiCol

student = Loesung { problem = DreiCol
		  , ident = Ident { aufgabe = 1 }
		  , beweis = bsp_Cfaerb 
		  }

bsp_Cfaerb = listToFM [(1,1),(2,2),(3,3),(4,1),(5,2)]
