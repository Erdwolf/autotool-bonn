{-# LANGUAGE DeriveDataTypeable #-}
module Syntax.Syntax where

import Data.Generics (Data, Typeable)

data Graph = Chain    Graph Graph
           | Fork     Graph Graph
           | Loop     Graph
           | Terminal String
           | Symbol   String
           | Empty
        deriving (Eq, Data, Typeable)

type Language = [(String, Graph)] -- The first entry is assumed to be the start symbol
