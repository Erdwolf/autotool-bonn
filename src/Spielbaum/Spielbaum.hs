module Spielbaum.Spielbaum where

import Graph.Graph               -- definition of Graph
import Graph.Viz                 -- getGraphviz

import Spielbaum.Trans
import Spielbaum.Type            -- definitions
import Spielbaum.Util            -- some useful things and show-functions
import Spielbaum.Test            -- some test functions
import Spielbaum.Next            
import Spielbaum.Wort

import List (inits, tails)
import Monad (guard)

-- erzeugt folgende Liste:  [ ( Knoten , Nachfolgerliste (Knoten) ) ]
order_nodes :: Graph ( Wort Char ) -> [ ( String , [ String ] ) ]
order_nodes graph = 
        let
            knotenliste = map inhalt $ setToList $ knoten graph
            childrenlst = map 
                             (map inhalt) 
                             (map3 childs graph (setToList $ knoten graph))
        in
            zip knotenliste childrenlst

-- aus der Liste ( Knoten, Nachfolger ( Knoten ) ) und der anfänglich leeren Liste
-- von SpielbaumLabels wird die Liste der SpielbaumLabel berechnet
renameNodes :: [ ( String , [ String ] ) ] -> [ SpielbaumLabel ] -> [ SpielbaumLabel ]
renameNodes ordered old  
       | ordered == [] = []
       | otherwise     = if (( snd $ head ordered) == [] )
                          then new_label
                                : renameNodes (tail ordered) (new_label:old)
                          else 
                             if ( berechenbares_label )
                               then (berechne_label) 
                                    : renameNodes (tail ordered) (berechne_label:old)
                               else (renameNodes ((tail ordered)++[head ordered]) old)
       where new_label = SpielbaumLabel { label  = fst $ head ordered
                                        , gv     = V
                                        , grundy = 0 }
             berechenbares_label = and (map2 is_in (snd $ head ordered) old)
             berechne_label = SpielbaumLabel { label  = fst $ head ordered
                                             , gv     = berechne_gv
                                             , grundy = berechne_gr }
             right_labels = map2 right_label (snd $ head ordered) old
             berechne_gr = mex ( map grundy right_labels )
             berechne_gv = if ( [G] == ( setToList $ mkSet ( map gv right_labels ) ) )
                            then V
                            else G

-- erzeugt aus der Knoten- und Nachfolgerliste und der Liste der SpielbaumLabel
-- die Liste der Kanten im Graphen, indem die Suchfunktion auf die Liste gemapt wird
getEdges :: [( String , [ String ] )] -> [SpielbaumLabel] -> [Kante SpielbaumLabel]
getEdges str_lst sLlist
        | str_lst == [] = []
        | otherwise     = (help (map2 right_label (snd $ head str_lst) sLlist))
                          ++ (getEdges (tail str_lst) sLlist)
        where
             help []     = []
             help (x:xs) = Kante { von = (right_label (fst $ head str_lst) sLlist)
                                 , nach = x }
                           : help xs

-- könnte eventuell eingespart werden
returnGameTree :: Wort Char -> Graph SpielbaumLabel
returnGameTree wort = 
        Graph { knoten = mkSet node_list
              , kanten = mkSet edge_list
              }
        where
             node_list = renameNodes child_lst []
             edge_list = getEdges child_lst node_list
             child_lst = order_nodes tree
             tree      = spielbaum wort

buildTree :: [ String ] -> String -> IO (String, GVFormat, ExitCode)
buildTree args option = do
          let
             rs   = do k <- [ 0, 2 .. length args - 2 ]
                       return $ Regel { from = args !! k, to   = args !! ( k + 1 ) }
             wort = Wort { inhalt = last args, regeln = rs }
             fname = ((last args) ++ (option))
          getGraphviz 
                  (returnGameTree wort) 
                  (myTrans option)
                  ((inhalt wort) ++ option)

mex :: [ Int ] -> Int
mex xs = head $ filter ( \ x -> not (elem x xs) ) [ 0 .. ]

right_label :: String -> [ SpielbaumLabel ] -> SpielbaumLabel
right_label str sLlist 
          | sLlist == []                    = undefined
          | ( str == (label $ head sLlist)) = head sLlist
          | otherwise                       = right_label str $ tail sLlist

is_in :: String -> [ SpielbaumLabel ] -> Bool
is_in str sLlist
          | sLlist == []                    = False
          | ( str == (label $ head sLlist)) = True
          | otherwise                       = is_in str $ tail sLlist

