module Col.Col 
--(Col,validiere, verifiziere) 
	where

import Graph.Type
import Graph.Util
import Challenger
import ToDoc
import Set
import Sort
import Monad ( guard ) -- old style

data Col = Col deriving Show
data Ergeb a = Ergeb [[a]] deriving (Read,Show)

instance (ToDoc [a])=> ToDoc (Ergeb a) where
	toDoc (Ergeb erg) = text "Loesung: " <+> toDoc erg


instance (ToDoc (Graph a), Show (Graph a), Read (Graph a), Iso (Graph a)
         , ToDoc (Ergeb a),Show (Ergeb a), Read (Ergeb a), Ord a, ToDoc a
         , Show a)
    => Problem Col (Graph a) (Ergeb a) where

    validiere Col g lsg = 
    		let 
              (kpassen, fehlknoten) = kantenPassenZuKnoten g
              zusammen = isZusammen g
            in  
              if and [kpassen, zusammen]
			    then (True, text "Graph Ok.")
			    else (False, 
                		maybe (text "Dein Graph ist kein zusammenhaengender Graph.")
                    		  (\x -> text ("In deinem Graph existiert mindestents eine Kante "
                          		      ++ "mit falschem Knoten: " ++ show x)
                              )
                          	  fehlknoten
                     )
    verifiziere Col g (Ergeb lsg) = colTest g lsg



--Probe, ob Knoten der Loesung mit Knoten des Graphen uebereinstimmen
colTest :: (ToDoc a, Ord a) => Graph a -> [[a]] -> (Bool,Doc)
colTest g b = 
  if not((sort(knotenliste g))==(sort(concat b)))
    then (False, text "Nicht alle oder zuviele Knoten sind in Deiner Loesung enthalten. Probiers nochmal")
    else if ([]==(klassenTest g b))
        then (True, text "Herzlichen Glueckwunsch, Deine Faerbung ist korrekt.")
        else (False, text "Deine Faerbung ist unkorrekt! Mindestens ein Knotenpaar ist in einer Klasse.")
--eigentliche Colorisierungs-Testfunktion
klassenTest :: (ToDoc a, Ord a, Eq a) => Graph a -> [[a]] -> [Int]
klassenTest g li = do
                  x <- (knotenliste g)
                  y <- (nachfolger g x)
                  guard $ not $ elem y (concat(listeohnex li x))
                  return 0

--aus einer liste einer liste wird eine liste geloescht^
listeohnex :: Eq a => [[a]] -> a -> [[a]]
listeohnex a b = do
                  z <- a
                  if (func1 z b == z)
                    then return z
                    else return []
                  
func1 :: Eq a => [a] -> a -> [a]
func1 z b = do
            y <- z
            guard $ not $ y==b
            return y

--
bsp_Cgraph = Graph {
    knoten = mkSet [1, 2, 3, 4, 5],
    kanten = mkSet [kante 1 2, kante 2 3, kante 3 4, kante 4 5, kante 3 5, kante 1 5]
}
bsp_Cfaerb = Ergeb [[1,4],[2,5],[3]]


--falsch
bsp_Cgraph2 = Graph {
    knoten = mkSet [1, 2, 3, 4, 5],
    kanten = mkSet [kante 1 2, kante 2 3, kante 3 4, kante 4 5, kante 3 5, kante 1 5]
}
bsp_Cfaerb2 = Ergeb [[2,4],[1,5],[3]]

