module Graceful.Test
	where

import Graceful.Graceful
import Graceful.Labeling
import Graph.Graph
import Graph.Viz

main =
	--writeFile "./graph.out" (getGraphviz Graceful bsp_einfach_g bsp_einfach_l)
	--transGtoGV bsp_einfach_g bsp_einfach_t
	getBeweis Graceful bsp_einfach_g bsp_einfach_l "graph"
	
-------------------------------------------------------------------------------
-- hier folgen Beispiele
-------------------------------------------------------------------------------

bsp_einfach_g = Graph {
    knoten = mkSet ["A", "B", "C", "D"],
    kanten = mkSet [kante "A" "B", kante "B" "C", kante "C" "D"]
}

bsp_einfach_l = mkLabeling [("A", 3), ("B", 0), ("C", 2), ("D", 1)]

bsp_einfach_t = GVTrans
	{ getGVNID = id
	, getGVNName = id
	, getGVNLabel = Just getNLabel
	, getGVNColor = Nothing
	, getGVNXAtts = Nothing
	, isGVEDirected = (\x -> False)
	, getGVELabel = Just getELabel
	, getGVEXAtts = Nothing
	}

getNLabel :: String -> GVLabel
getNLabel knoten = '(' : shows (getLabel bsp_einfach_l knoten) ")"

getELabel :: (Kante String) -> GVLabel
getELabel kante = show (getDiff bsp_einfach_l kante)

bsp_graph_g = Graph 
	{ knoten = mkSet ["Middle", "North", "West", "East"]
	, kanten = mkSet
		[ kante "North" "Middle"
		, kante "West" "Middle"
		, kante "East" "Middle"
		, kante "North" "West"
		, kante "West" "East"
		, kante "East" "North"
		]
	}

bsp_graph_l = mkLabeling
	[ ("Middle", 6), ("North", 4), ("West", 0), ("East", 1)]

bsp_krebs_g = Graph
	{ knoten = mkSet [0..11]
	, kanten = mkSet
		[ kante 10 0
		, kante 9 0
		, kante 0 11
		, kante 11 3
		, kante 3 8
		, kante 8 1
		, kante 8 2
		, kante 3 6
		, kante 6 7
		, kante 7 4
		, kante 7 5
		]
	}

bsp_krebs_l = mkLabeling[(a, a) | a <- [0..11]]



