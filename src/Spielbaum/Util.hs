module Spielbaum.Util where

import Graph.Graph
import Spielbaum.Type
import Spielbaum.Wort

import Control.Monad(guard)

-- map2 :: (a -> b -> c) -> [a] -> b -> [c]
map2 :: (a -> [b] -> c) -> [a] -> [b] -> [c]
map2 f []     b = []
map2 f (x:xs) b = f x b : map2 f xs b

map3 :: (Graph a -> a -> [a]) -> Graph a -> [a] -> [[a]]
map3 f graph []     = []
map3 f graph (x:xs) = f graph x : map3 f graph xs

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

-- to see the properly formated string in terminal type:
-- putStr showEdges <...>
showEdges :: [Kante SpielbaumLabel] -> String
showEdges kantenListe
        | (kantenListe == []) = ""
        | otherwise           = "Kante: von " ++ (label $ von $ head kantenListe)
                                ++ " nach " ++ (label $ nach $ head kantenListe)
                                ++ "\n" ++ (showEdges $ tail kantenListe)

-- this function is just nice:
-- :t map    = (a -> b) -> [a] -> [b]
-- means: use function (a -> b) on every element of [a] and get [b]
-- :t foldr1 = (a -> a -> a) -> [a] -> a
-- means: in this example: (a ++ (a ++ (a ++ (...))))
showWort :: Wort Char -> IO ()
showWort wort = do
	  putStrLn ("   Inhalt: " ++ (inhalt wort))
          putStr   ((foldr1 (++) (map showRegel (regeln wort))))

showRegel :: Regel Char -> String
showRegel rule = "   Regel: " ++ (from rule) ++ " -> " ++ (to rule) ++ " \n"

showNodeChildren :: Graph ( Wort Char ) -> [[Wort Char]]
showNodeChildren graph = map3 childs graph (setToList $ knoten graph)

-- childrenlist of node in graph -> should be defined in Graph.Util
-- but function imported from there doesn't work properly
childs :: Eq a => Graph a -> a -> [ a ]
childs graph node = do
     k @ Kante { } <- setToList $ kanten graph
     guard $ von k == node
     return $ nach k

-- QuickSort
qSort :: [ Int ] -> [ Int ]
qSort []     = []
qSort (x:xs) = qSort [ y | y <- xs, y <= x] ++ [ x ] ++ qSort [ y | y <- xs , y > x]
-- informal, but does the same: setToList.mkSet
-- reason: set a = finiteMap a ()
