{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Robots3.Generator where

import Robots3.Config
import Robots3.Data
import Robots3.Solver
import Robots3.Nice

import Autolib.Util.Splits
import Autolib.Util.Zufall
import Autolib.ToDoc

import Robots3.Quiz
import Inter.Quiz

import Data.List ( intersperse )
import Data.Array ( range )

import System.IO
import Data.IORef

someconf :: Int -> Int -> [ Position ] -> IO Config
someconf n t pos = do
    psts <- selektion (n + t) pos
    let ( ps, ts ) = splitAt n psts
    let rob (c, p) = Robot { name = [c] , position = p }
    let rs = map rob $ take n $ zip [ 'A' .. ] ps
    return $ Robots3.Config.make rs ts

sol :: Int -> Int -> Int -> [ Position ] -> IO ( Config, [[Zug]] )
sol sw n t pos = do
    c <- someconf n t pos
    return ( c, shortest' sw c )

instance Generator Robots3 RC ( Config, [Zug] ) where
    generator p rc key = do
       let w = width rc
           u = negate w
	   pos = range (Position u u, Position w w )
       ( i, zss ) <- sol (search_width rc) (num_robots rc) (num_targets rc) pos
           `repeat_until` \ ( c, zss ) -> case zss of
	       []      -> False
	       zs : _  -> at_least rc <= length zs
       return ( i, head zss )

instance Project Robots3 ( Config, [Zug] ) Config where
   project p ( c, zs ) = c


instance Generator Robots3_Inverse RC ( [Zug], Config ) where
    generator p rc key = do
       let w = width rc
           u = negate w
	   pos = range (Position u u, Position w w )
       ( i, zss ) <- sol (search_width rc) (num_robots rc) (num_targets rc) pos
           `repeat_until` \ ( c, zss ) -> case zss of
	       []      -> False
	       zs : _  -> at_least rc <= length zs
       return ( head zss , i )

instance Project Robots3_Inverse ( [Zug], Config ) [Zug] where
   project p ( zs, c ) = zs


