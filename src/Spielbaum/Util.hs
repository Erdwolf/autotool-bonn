module Spielbaum.Util where

import Graph.Graph
import Spielbaum.Type

showSpielbaumLabel :: SpielbaumLabel -> String
showSpielbaumLabel sl = label sl ++ " " ++ (show $ gv sl) 
                        ++ " " ++ (show $ grundy sl) -- ++ "\n"

showGraphSpielbaumLabel :: Graph SpielbaumLabel -> Graph String
showGraphSpielbaumLabel graph = 
         Graph { knoten = mkSet $ map sL $ setToList $ knoten graph
               , kanten = mkSet $ map sX $ setToList $ kanten graph }
         where sX kante = Kante { von  = sL $  von kante
                                , nach = sL $  nach kante }
               sL knoten = showSpielbaumLabel knoten

-- um dann im Terminal das ganze richtig zu sehen:
-- putStr showEdges <...>
showEdges :: [Kante SpielbaumLabel] -> String
showEdges kantenListe
        | (kantenListe == []) = ""
        | otherwise           = "Kante: von " ++ (label $ von $ head kantenListe)
                                ++ " nach " ++ (label $ nach $ head kantenListe)
                                ++ "\n" ++ (showEdges $ tail kantenListe)

-- QuickSort
qSort :: [ Int ] -> [ Int ]
qSort []     = []
qSort (x:xs) = qSort [ y | y <- xs, y <= x] ++ [ x ] ++ qSort [ y | y <- xs , y > x]
