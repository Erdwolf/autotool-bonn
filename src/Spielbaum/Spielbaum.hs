module Spielbaum where

import Graph.Graph               -- definition of Graph
import Graph.Type  -- wofür???
import Graph.Viz                 -- getGraphviz

import Spielbaum.Trans(myTrans)
import Spielbaum.Type
import Spielbaum.Util
-- import Spielbaum.Test


-- import ToDoc                     -- for instance ToDoc (like show??)
-- import Graph.Util
import List (inits, tails)
import Monad (guard)
-- import Schichten     
-- import FiniteMap

-- und das geht deshalb nicht, weil der die falsche importiert:
-- /autotool/util/Wort.hs, aber ich brauche:
-- /autotool/autobahn/Wort.hs
-- import Wort             
import Spielbaum.Next

-- import Spielbaum.Test
------------------------------------------------------------------------
-- some test-things: die sollen nach Spielbaum.Test, aber dann fehlt wort
------------------------------------------------------------------------

a_wort :: Wort Char
a_wort = Wort { inhalt = "011"
              , regeln = [ Regel { from = "01" , to = "10" } 
                         , Regel { from = "1"  , to = "2"  }
                         ]
              }

b_wort :: Wort Char
b_wort = Wort { inhalt = "0101"
              , regeln = [ Regel { from = "01" , to = "10" } 
                         , Regel { from = "1"  , to = ""  }
                         ]
              }

------------------------------------------------------------------------
-- END: some test-things
------------------------------------------------------------------------
{-
-- wende Funktion (a -> b) auf alle Elemente der Liste a an und
-- erzeuge eine Liste vom Typ b
-- map :: (a -> b) -> [a] -> [b]

-- wende die Funktion n2sl ( String -> Graph String -> SpielbaumLabel )
-- auf alle Elemente der Liste von Knoten (Strings) an und erzeuge
-- eine Liste vom Typ SpielbaumLabel
own :: (a -> b -> c) -> [a] -> b -> [c]
own f [] g = []
own f (x:xs) g = f x g : own f xs g

find_sl :: String -> [ SpielbaumLabel ] -> SpielbaumLabel
find_sl s_label (fst_sl :  tail_sl) 
   | (fst_sl:tail_sl) == [] = undefined
   | s_label ==label fst_sl = fst_sl
   | otherwise              = find_sl s_label tail_sl

ops :: [ GV ] -> GV
ops ls = case ( setToList $ mkSet ls ) of
          [G] -> V
          [X] -> X
          _   -> G

gvl :: [ SpielbaumLabel ] -> [ GV ]
gvl sll -- (sl:rest) 
   | sll==[]          = [ V ]
   | ([] == tail sll) = [ gv $ head sll ]
   | otherwise        = ( gv $ head sll ) : (gvl $ tail sll)

-- make gv-decision:
gvd :: SpielbaumLabel -> Graph SpielbaumLabel -> SpielbaumLabel
gvd sl graph = 
    if ( [] == nfl_lst )
     then sl
     else SpielbaumLabel { label  = label sl
                         , gv     = ops $ gvl nfl_lst
                         , grundy = grundy sl }
    where nfl_lst = nfl2 graph sl

grv3 :: [ SpielbaumLabel ] -> Graph SpielbaumLabel -> Graph SpielbaumLabel
grv3 sl_list graph = 
     Graph { knoten = mkSet sl_list
           , kanten = mkSet $ grv4 xy sl_list
           }
     where 
          xy = setToList $ kanten graph

-- if Kante in xy
-- pro Kante in xy soll Kante aus sl_list erzeugt werden

grv5 :: [ SpielbaumLabel ] -> String -> SpielbaumLabel
grv5 ll str
   | str == (label $ head ll) = head ll
   | otherwise                = grv5 (tail ll) str

grv4 :: [ Kante SpielbaumLabel ] -> [ SpielbaumLabel ] -> [ Kante SpielbaumLabel ]
grv4 alt neu 
   | alt == [] = []
   | otherwise = ( Kante { von  = grv5 neu $ label $ von  $ head alt
                         , nach = grv5 neu $ label $ nach $ head alt
                         }
                 ) : grv4 (tail alt) neu

new_childrenlist :: [ SpielbaumLabel ] -> [ SpielbaumLabel ] -> [ SpielbaumLabel ]
new_childrenlist graph_list grundy_list 
   | grundy_list == [] = []
   | graph_list == [] = []
   | (label $ head grundy_list) == (label $ head graph_list)
                       = (head grundy_list) : 
                         new_childrenlist (graph_list) (tail grundy_list)
   | (tail grundy_list) == []
                       = new_childrenlist (tail graph_list) [ head grundy_list ]
   | otherwise         = (new_childrenlist (tail graph_list) [ head grundy_list ])
                         ++ (new_childrenlist graph_list (tail grundy_list))

grl :: [ SpielbaumLabel ] -> [ Int ]
grl sl_lst 
   | sl_lst == [] = [ 0 ]
   | (rest == []) = [ grundy sl ]
   | otherwise    =   grundy sl : grl rest
   where (sl:rest) = sl_lst

min_list :: [ Int ] -> Int
min_list i_lst
   | i_lst == []        = undefined
   | (tail i_lst) == [] = head i_lst
   | otherwise          = min_list ( ( min (head i_lst) (head $ tail i_lst) )
                          : ( tail $ tail i_lst ) )

-- null-fall fehlt!!!
grd :: [ Int ] -> Int
grd (i:is)
    | i == -1                  = -1
    | 0 /= min_list (i:is)     = 0
    | otherwise                = grs (i:is)

grs :: [ Int ] -> Int
grs (i:is)
    | is == []                 = 1 + i
    | ( i + 1 ) == ( head is ) = grs is
    | otherwise                = 1 + i

gre :: [ Int ]
gre = [-1,-1,0,-1,0,-1,2,3,6,7,1,4]

-----------------------------------------------------------------------
-- abhängig vom RICHTIGEN graph bauen
-----------------------------------------------------------------------
nf2 :: Kante String -> [ SpielbaumLabel ] -> Kante SpielbaumLabel
nf2 k sl = Kante { von = find_sl (von k) sl
                 , nach = find_sl (nach k) sl
                 }

nf6 :: Graph String -> [ SpielbaumLabel ]
nf6 graph = own n2sl (setToList $ knoten graph) graph



own2 :: (a -> [b] -> c) -> [a] -> [b] -> [c]
own2 f [] ll     = []
own2 f (x:xs) ll = f x ll : own f xs ll

showKanteSpielbaumLabel :: Kante SpielbaumLabel -> String
showKanteSpielbaumLabel k = "Kante " ++ (show $ von k) 
                            ++ " nach " ++ (show $ nach k)

sKSLl :: [ Kante SpielbaumLabel ] -> String
sKSLl (x:xs) 
   | xs == []  =  showKanteSpielbaumLabel x
   | otherwise = (showKanteSpielbaumLabel x) ++ sKSLl xs

gsl2gs :: Graph SpielbaumLabel -> Graph String
gsl2gs graph =
    Graph { knoten = mkSet $ map sL $ setToList $ knoten graph 
          , kanten = mkSet $ map sX $ setToList $ kanten graph
          }
    where sX k = Kante { von = sL $ von k , nach = sL $ nach k }

-- convert Graph ( Wort Char ) to Graph String
gwc2gs :: Graph ( Wort Char ) -> Graph String
gwc2gs graph = Graph { knoten = mkSet $ map inhalt $ setToList $ knoten graph
                     , kanten = mkSet $ map fs $ setToList $ kanten graph }
       where fs k = Kante { von = inhalt $ von k , nach = inhalt $ nach k }

sL :: SpielbaumLabel -> String
sL = showSpielbaumLabel

-- instance ToDoc ( SpielbaumLabel ) => Show ( SpielbaumLabel ) where
--     show = render.toDoc

{- do
   ( _ , _ , exitCode ) <- getGraphviz ( graph ) myTrans file
      if exitCode == ExitSuccess
         then do
            system command
            parsed_things <- parse outFile
            return (listToFM $ zip xs $ parsed_things)
      else error "Problem mit dem Graph-Viz"
-}
-}
------------------------------------------------------------------------
-- WORT konnte ich nicht importieren!!
------------------------------------------------------------------------

data Regel a = 
     Regel { from :: [a]
	   , to :: [a]
	   }

data Wort a = Wort { inhalt :: [a]
		   , regeln :: [ Regel a ]
		   }

instance ToDoc [a] => ToDoc (Wort a) where
    toDoc = toDoc . inhalt

instance ToDoc (Wort a) => Show (Wort a) where
    show = render . toDoc

instance Eq [a] => Eq (Wort a) where
    x == y = inhalt x == inhalt y 

instance Ord [a] => Ord (Wort a) where
    x `compare` y = inhalt x `compare` inhalt y 

instance Eq a => Next (Wort a) where
    next w = do
	let i = inhalt w
        ( pre, midpost ) <- zip ( inits i ) ( tails i )
	r <- regeln w
	let ( mid, post ) = splitAt (length $ from r) midpost
	guard $ mid == from r
	return $ w { inhalt =  pre ++ to r ++ post }

--------------------------------------------------------------------
-- restart:
--------------------------------------------------------------------

-----------------------------
-- test and show functions --
-----------------------------

-- function for testing
showWort :: Wort Char -> IO ()
showWort wort = do
	  putStrLn ("   Inhalt: " ++ (inhalt wort))
          putStr   ((foldr1 (++) (map showRegel (regeln wort))))

-- function for testing
showRegel :: Regel Char -> String
showRegel rule = "   Regel: " ++ (from rule) ++ " -> " ++ (to rule) ++ " \n"

showNodeChildren :: Graph ( Wort Char ) -> [[Wort Char]]
showNodeChildren graph = map3 childs graph (setToList $ knoten graph)

map3 :: (Graph a -> a -> [a]) -> Graph a -> [a] -> [[a]]
map3 f graph []     = []
map3 f graph (x:xs) = f graph x : map3 f graph xs

-- was dann gebraucht wird:  [ ( Knoten , Nachfolgerliste (Knoten) ) ]
order_nodes :: Graph ( Wort Char ) -> [ ( String , [ String ] ) ]
order_nodes graph = 
        let
            knotenliste = map inhalt $ setToList $ knoten graph
            childrenlst = map 
                             (map inhalt) 
                             (map3 childs graph (setToList $ knoten graph))
        in
            zip knotenliste childrenlst

-- ich brauche als nächstes die Spielbaumlabel-Knotenliste (erstmal)
-- die will ich gleich aus order_nodes bauen. dazu sollen erstmal alle
-- knoten zu spielbaumknoten werden, und 
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
{-                          
asdf :: Wort Char -> String
asdf wort = showEdges ( getEdges ( order_nodes $ spielbaum wort)
                                 ( renameNodes ( order_nodes $ spielbaum wort ) [] ) )
-}
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

gameTreeToStringTree :: Graph SpielbaumLabel -> Graph String
gameTreeToStringTree graph =
        Graph { knoten = mkSet $ map sL $ setToList $ knoten graph
              , kanten = mkSet $ map sX $ setToList $ kanten graph }
    where 
           sX k = Kante { von = sL $ von k , nach = sL $ nach k }
           sL k = ((label k) ++ "\n" ++ (show $ gv k) ++ " : " ++ (show $ grundy k))

tr :: Wort Char -> IO (String, GVFormat, ExitCode)
tr wort = getGraphviz 
                (gameTreeToStringTree $ returnGameTree wort) 
                myTrans 
                (inhalt wort)

mex :: [ Int ] -> Int
mex xs = head $ filter ( \ x -> not (elem x xs) ) [ 0 .. ]

right_label :: String -> [ SpielbaumLabel ] -> SpielbaumLabel
right_label str sLlist 
          | sLlist == []                    = undefined
          | ( str == (label $ head sLlist)) = head sLlist
          | otherwise                       = right_label str $ tail sLlist

map2 :: (a -> [b] -> c) -> [a] -> [b] -> [c]
map2 f []     b = []
map2 f (x:xs) b = f x b : map2 f xs b

is_in :: String -> [ SpielbaumLabel ] -> Bool
is_in str sLlist
          | sLlist == []                    = False
          | ( str == (label $ head sLlist)) = True
          | otherwise                       = is_in str $ tail sLlist

-- childrenlist of node in graph
-- this should be defined in Graph.Util but
-- function there doesn't work
childs :: Eq a => Graph a -> a -> [ a ]
childs graph node = do
     k @ Kante { } <- setToList $ kanten graph
     guard $ von k == node
     return $ nach k

