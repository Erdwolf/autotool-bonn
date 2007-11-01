{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Robots.Generator where

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

import System.IO
import Data.IORef

-- | erzeugt eine konfiguration mit n robots,
-- alle im bereich (-w, -w) .. (+w, +w)
some_without_target :: Int -> [(Integer,Integer)] -> IO Config
some_without_target n pos = do
    let rob (c, p) = Robot { name = [c] , position = p, ziel = Nothing }
    ps <- selektion n pos
    let rs = map rob $ take n $ zip [ 'A' .. ] ps
    return $ Robots.Config.make rs

-- | mit Ziel
some n pos = do
    c <- some_without_target n pos
    i <- randomRIO (0, n-1)
    let ( pre, x : post ) = splitAt i $ robots c
    tgt <- eins pos
    return $ Robots.Config.make 
           $ pre ++ [ x { ziel = Just tgt } ] ++ post

some0 n pos = do
    let rob (c, p) = Robot { name = [c] , position = p, ziel = Nothing }
    tgt : ps <- selektion (succ n) pos
    let rs = map rob $ take n $ zip [ 'A' .. ] ps
    i <- randomRIO (0, n-1)
    let ( pre, x : post ) = splitAt i rs
    return $ Robots.Config.make $ pre ++ [ x { ziel = Just (0,0) } ] ++ post

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


instance Generator Robots_Inverse RC ( [Zug], Config ) where
    generator p rc key = do
       let w = width rc
	   pos = range ((-w,-w), (w,w))
       ( i, zss ) <- sol (search_width rc) (num rc) pos
           `repeat_until` \ ( c, zss ) -> case zss of
	       []      -> False
	       zs : _  -> at_least rc <= length zs
       return ( head zss , i )

instance Project Robots_Inverse ( [Zug], Config ) [Zug] where
   project p ( zs, c ) = zs


