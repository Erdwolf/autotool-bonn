{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving, NoMonomorphismRestriction #-}
module Generics where

import Syntax.Syntax

import Data.Generics


deriving instance Typeable Graph
deriving instance Data Graph

terminals = everything (++) ([] `mkQ` q)
 where
   q (Terminal t) = [t]
   q _            = []

symbols = everything (++) ([] `mkQ` q)
 where
   q (Symbol s) = [s]
   q _          = []

