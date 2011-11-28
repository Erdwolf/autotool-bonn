module HeapSort.Operation where

data Operation = Sinken Knoten [Richtung]
               | Tauschen Knoten Knoten

data Richtung = L | R

type Knoten = Int
