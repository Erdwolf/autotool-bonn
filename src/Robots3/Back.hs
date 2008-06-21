{-# language PatternSignatures #-}

module Robots3.Back where

import Robots3.Config
import Robots3.Solver
import Robots3.Move
import Robots3.Quiz
import Robots3.Data
import Robots3.Nice
import Robots3.Generator

import Inter.Quiz

import Autolib.ToDoc
import Autolib.Util.Zufall ( selektion )

import qualified Data.Set as S
import Data.Set ( Set )

import Control.Monad ( when )
import Data.Ix ( range )

p :: RC
p = RC { width = 3
	, num_robots = 5
	, num_targets = 2
	, at_least = 5
	, search_width = 10000
	}

main = sequence_ $ repeat $ handle p

handle p =  do
    let w = width p
        u = negate w
        pos = range (Position u u, Position w w )
    c0 <- someconf ( num_robots p ) 0 pos
    ts <- selektion ( num_targets p ) $ positions c0
    let c = c0 `with_targets` ts 
    let cs = searcher $ S.singleton (0, [], c)
    mapM_ ( \ (n,sol,c)-> do print n ; print sol ; print ( nice c ) ) 
          $ drop 5
          $ rising_by ( \ (n,sol,c) -> n ) cs

rising_by f [] = []
rising_by f (x:xs) = x : rising_by f ( filter ( \ y -> f y > f x) xs )

searcher todo = case S.maxView todo of
    Nothing -> []
    Just ( this @ (w, tsol, t), odo ) -> 
        let next = do
                s <- direct_predecessors t
                case shortest s of
                    sol : _ | length sol > w -> [ (length sol, sol, s) ]
                    _ -> []
        in  this : searcher ( S.union odo $ S.fromList next )



