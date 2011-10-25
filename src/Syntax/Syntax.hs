module Syntax.Syntax where

data Graph = Chain    Graph Graph
           | Fork     Graph Graph
           | Loop     Graph
           | Terminal String
           | Symbol   String
           | Empty
        deriving Eq

type Language = [(String, Graph)] -- The first entry is assumed to be the start symbol
