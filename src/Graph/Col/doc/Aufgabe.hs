-- -*- mode: haskell -*-

import Challenger
import Col.DreiCol

student = Aufgabe {  problem = DreiCol
		  , instanz = bsp_Cgraph 
		  , beweis = bsp_Cfaerb 
		  }

bsp_Cgraph = Graph {
    knoten = mkSet [1, 2, 3, 4, 5],
    kanten = mkSet [kante 1 2, kante 2 3
		   , kante 3 4, kante 4 5, kante 3 5, kante 1 5
		   ]
    }

bsp_Cfaerb = listToFM [(1,1),(2,2),(3,3),(4,1),(5,2)]

