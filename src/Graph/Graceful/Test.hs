module Graceful.Test
	where

import Graceful.Graceful
import Graceful.Labeling
import Graph.Graph
import Graph.Viz

main =
	--writeFile "./graph.out" (getGraphviz Graceful bsp_einfach_g bsp_einfach_l)
	--transGtoGV bsp_einfach_g bsp_einfach_t
	--getBeweis Graceful bsp_einfach_g bsp_einfach_l "graph"
	--getBeweis Graceful bsp_graph_g bsp_graph_l "graph"
	--getBeweis Graceful bsp_krebs_g bsp_krebs_l "krebs"
	--validiere Graceful bsp_graph_g bsp_graph_l
	verifiziere Graceful bsp_graph_g bsp_graph_l
	
-------------------------------------------------------------------------------
-- hier folgen Beispiele
-------------------------------------------------------------------------------

bsp_einfach_g = Graph {
    knoten = mkSet ["A", "B", "C", "D"],
    kanten = mkSet [kante "A" "B", kante "B" "C", kante "C" "D"]
}

bsp_einfach_l = mkLabeling [("A", 3), ("B", 0), ("C", 2), ("D", 1)]

bsp_graph_g = Graph 
	{ knoten = mkSet ['B', 'C', 'A', 'D']
	, kanten = mkSet
		[ kante 'C' 'B'
		, kante 'A' 'B'
		, kante 'D' 'B'
		, kante 'C' 'A'
		, kante 'A' 'D'
		, kante 'D' 'C'
		]
	}

bsp_graph_l = mkLabeling
	[ ('B', 6), ('C', 4), ('A', 0), ('D', 1)]

{-bsp_krebs_g = Graph
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
	}-}

bsp_krebs_g = Graph
	{ knoten = mkSet [0,1,2,3,4,5,6,7,8,9,10,11]
	, kanten = mkSet
		[ kante 10 0
		, kante 9 0
		, kante 0 11
		, kante 11 3
		, kante 3 8
		, kante 8 1
		, kante 8 2
		, kante 3 7
		, kante 7 4
		, kante 4 5
		, kante 4 6
		]
	}

bsp_krebs_l = mkLabeling
	[ (0,0)
	, (1,1)
	, (2,2)
	, (2,2)
	, (3,3)
	, (4,4)
	, (5,5)
	, (6,6)
	, (7,7)
	, (8,8)
	, (9,9)
	, (10,10)
	, (11,11)
	]



