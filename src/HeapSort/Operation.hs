module HeapSort.Operation where

data Operation = S Knoten [Richtung]
               | T (Knoten, Knoten)

data Richtung = L | R

type Knoten = Int
