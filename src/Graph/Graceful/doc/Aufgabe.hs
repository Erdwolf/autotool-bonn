import Challenger
import Graceful.Graceful


student = Aufgabe {problem = Graceful,
	 instanz = Graph {knoten  = mkSet ["V1", "V10", "V11", "V12", "V13",
					   "V14", "V15", "V16", "V2", "V3", "V4", "V5", "V6", "V7",
					   "V8", "V9"],
			  kanten  = mkSet [kante "V1" "V12", kante "V1" "V2",
					   kante "V1" "V7", kante "V10" "V11", kante "V12" "V13",
					   kante "V12" "V15", kante "V13" "V14", kante "V15" "V16",
					   kante "V2" "V3", kante "V2" "V5", kante "V3" "V4",
					   kante "V5" "V6", kante "V7" "V10", kante "V7" "V8",
					   kante "V8" "V9"]},
	 beweis = listToFM [("V1", 0), ("V10", 6), ("V11", 9),
			      ("V12", 15), ("V13", 3), ("V14", 12), ("V15", 1), ("V16", 14),
			      ("V2", 5), ("V3", 13), ("V4", 2), ("V5", 11), ("V6", 4),
			      ("V7", 10), ("V8", 8), ("V9", 7)]}