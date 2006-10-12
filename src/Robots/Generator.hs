{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Robots.Generator where

--  $Id$

import Robots.Config
import Robots.Data
import Robots.Solver
import Robots.Nice

import Autolib.Util.Splits
import Autolib.Util.Zufall
import Autolib.ToDoc

import Robots.Quiz
import Inter.Quiz

import Data.List ( intersperse )
import Data.Array ( range )

import System.Random
import IO
import Data.IORef

-- | erzeugt eine konfiguration mit n robots,
-- alle im bereich (-w, -w) .. (+w, +w)
some :: Int -> [(Integer,Integer)] -> IO Config
some n pos = do
    let rob (c, p) = Robot { name = [c] , position = p, ziel = Nothing }
    tgt : ps <- selektion (succ n) pos
    let rs = map rob $ take n $ zip [ 'A' .. ] ps
    i <- randomRIO (0, n-1)
    let ( pre, x : post ) = splitAt i rs
    return $ Robots.Config.make $ pre ++ [ x { ziel = Just tgt } ] ++ post

sol :: Int -> Int -> [(Integer,Integer)] -> IO ( Config, [[Zug]] )
sol sw n pos = do
    c <- some n pos
    return ( c, shortest' sw c )

instance Generator Robots RC ( Config, [Zug] ) where
    generator p rc key = do
       let w = width rc
	   pos = range ((-w,-w), (w,w))
       ( i, zss ) <- sol (search_width rc) (num rc) pos
           `repeat_until` \ ( c, zss ) -> case zss of
	       []      -> False
	       zs : _  -> at_least rc <= length zs
       return ( i, head zss )

instance Project Robots ( Config, [Zug] ) Config where
   project p ( c, zs ) = c

