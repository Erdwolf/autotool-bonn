module Robots.Solver where

--  $Id$

import Robots.Config
import Robots.Data
import Robots.Move
import Robots.Final
import Robots.Hull
import Robots.Nice
import Robots.Final
import Robots.Examples

import Autolib.Schichten

import Data.Maybe
import Autolib.Set
import Autolib.ToDoc
import Autolib.Reporter ( export )
import Control.Monad ( guard )


nachfolger :: Config -> [ Config ]
nachfolger k = do
    let ( t, _ :: Doc ) = export $ valid k
    guard $ isJust t
    r <- robots k
    d <- richtungen
    let z = (name r, d)
    k' <- maybeToList $ execute k z
    guard $ covered k'
    return k'

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
	 $ takeWhile ( \ zss -> cardinality zss < 100 )
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
ex = Robots.Config.make 
     [ Robot { name = "A", position = ( -2, -2 ) , ziel = Nothing }
       , Robot { name = "B", position = ( 2, 3 ), ziel = Just ( 0, 0 ) }
       , Robot { name = "C", position = ( -2, 1 ), ziel = Nothing }
       , Robot { name = "D", position = ( 2, 0 ), ziel = Nothing }
       , Robot { name = "E", position = ( -3, 1 ), ziel = Nothing }
       ]