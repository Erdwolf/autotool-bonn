module Syntax.Syntax where

data Graph = Chain    Graph Graph
           | Fork     Graph Graph
           | Loop     Graph
           | Terminal String
           | Symbol   String
           | Empty


