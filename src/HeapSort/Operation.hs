module HeapSort.Operation where

data Operation = S Knoten [Richtung]
               | T (Knoten, Knoten)

instance Show Operation where
    show (S k rs) = "S(" ++ show k ++ ") " ++ show rs
    show (T pair) = "T" ++ show pair

data Richtung = L | R deriving Show

type Knoten = Int
