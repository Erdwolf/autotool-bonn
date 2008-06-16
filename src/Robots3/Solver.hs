{-# OPTIONS -fglasgow-exts #-}

module Robots3.Solver where

--  $Id$

import Robots3.Config
import Robots3.Data
import Robots3.Move
import Robots3.Final
import Robots3.Hull
import Robots3.Nice
import Robots3.Final
import Robots3.Examples

import Autolib.Schichten

import Data.Maybe
import Autolib.Set
import Autolib.ToDoc
import Autolib.Reporter ( export )
import Control.Monad ( guard )

nachfolger :: Config -> [ Config ]
nachfolger k = do
    ( z, k' ) <- znachfolger k
    return k'

znachfolger :: Config -> [ ( Zug, Config ) ]
znachfolger k = do
    let ( t, _ :: Doc ) = export $ valid k
    guard $ isJust t
    r <- robots k
    d <- richtungen
    let z = (name r, d)
    k' <- maybeToList $ execute k z
    guard $ covered k'
    return ( z, k' )

reachables :: Config -> [[ Config ]]
reachables k = map setToList $ schichten ( mkSet . nachfolger ) k

solutions :: Config -> [ ((Int, Int), [[Zug]]) ]
solutions k = do
    (d, ps) <- zip [0 :: Int ..] $ schichten ( mkSet . nachfolger ) k
    let out = do p <- setToList ps
                 let ( t, _ :: Doc ) = export $ final p 
                 guard $ isJust t 
		 return $ reverse $ geschichte p
    return $ (( d, length out), filter (not . null) out)


solutions' :: Int -> Config -> [ ((Int, Int), [[Zug]]) ]
solutions' sw k = do
    (d, ps) <- zip [0 :: Int ..] 
	 $ takeWhile ( \ zss -> cardinality zss < sw )
	 $ schichten ( mkSet . nachfolger ) k
    let out = do p <- setToList ps
                 let ( t, _ :: Doc ) = export $ final p 
                 guard $ isJust t 
		 return $ reverse $ geschichte p
    return $ (( d, length out), filter (not . null) out)

solve :: Config -> IO ()
solve k = sequence_ $ map ( print . toDoc ) $ solutions k

shortest :: Config -> [[Zug]]
shortest k = take 1 $ do
    ( _ , zss ) <- solutions k
    zss

shortest' :: Int -> Config -> [[Zug]]
shortest' sw k = take 1 $ do
    ( _ , zss ) <- solutions' sw k
    zss

vorganger :: Config -> [ Config ]
vorganger c = do
    r <- robots c
    d <- richtungen
    let z = (name r, d)
    reverse_executes c z

ancestors :: Config -> [[Config]]
ancestors c = map setToList
	    $ schichten ( mkSet . vorganger ) c

----------------------------------------------------

ex :: Config
ex = Robots3.Config.make 
     [ Robot { name = "A", position = Position {x= -2, y = -2 }  }
       , Robot { name = "B", position = Position {x=  2, y = 3 }  }
       , Robot { name = "C", position = Position {x=  -2, y = 1 } }
       , Robot { name = "D", position = Position {x=  2, y = 0 } }
       , Robot { name = "E", position = Position {x=  -3, y = 1 } }
       ]
     [ Position {x= 0, y =0 } ]