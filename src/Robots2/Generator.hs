{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Robots2.Generator where

import Robots2.Config
import Robots2.Data
import Robots2.Solver
import Robots2.Nice

import Autolib.Util.Splits
import Autolib.Util.Zufall
import Autolib.ToDoc

import Robots2.Quiz
import Inter.Quiz

import Data.List ( intersperse )
import Data.Array ( range )

import System.IO
import Data.IORef

-- | erzeugt eine konfiguration mit p robots und t zielen,
-- alle im bereich (-w, -w) .. (+w, +w)
some p t pos = do
    ps <- selektion p pos
    ts <- selektion t pos
    return $ Robots2.Config.make ps ts
          
sol :: Int -> Int -> Int -> [(Integer,Integer)] -> IO ( Config, [[Zug]] )
sol sw n t pos = do
    c <- some n t pos
    return ( c, shortest' sw c )

instance Generator Robots RC ( Config, [Zug] ) where
    generator p rc key = do
       let w = width rc
	   pos = range ((-w,-w), (w,w))
       ( i, zss ) <- sol (search_width rc) (num_robots rc) (num_targets rc) pos
           `repeat_until` \ ( c, zss ) -> case zss of
	       []      -> False
	       zs : _  -> at_least rc <= length zs
       return ( i, head zss )

instance Project Robots ( Config, [Zug] ) Config where
   project p ( c, zs ) = c


instance Generator Robots_Inverse RC ( [Zug], Config ) where
    generator p rc key = do
       let w = width rc
	   pos = range ((-w,-w), (w,w))
       ( i, zss ) <- sol (search_width rc) (num_robots rc) (num_targets rc) pos
           `repeat_until` \ ( c, zss ) -> case zss of
	       []      -> False
	       zs : _  -> at_least rc <= length zs
       return ( head zss , i )

instance Project Robots_Inverse ( [Zug], Config ) [Zug] where
   project p ( zs, c ) = zs


