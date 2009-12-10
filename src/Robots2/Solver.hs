module Robots2.Solver where

--  $Id$

import Robots2.Config
import Robots2.Data
import Robots2.Move
import Robots2.Final
import Robots2.Hull
import Robots2.Nice
import Robots2.Final
import Robots2.Examples

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
    r <- positions k
    d <- richtungen
    let z = (r, d)
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

{-
vorganger :: Config -> [ Config ]
vorganger c = do
    r <- robots c
    d <- richtungen
    let z = (name r, d)
    reverse_executes c z

ancestors :: Config -> [[Config]]
ancestors c = map setToList
	    $ schichten ( mkSet . vorganger ) c
-}

----------------------------------------------------

ex :: Config
ex = Robots2.Config.make [ (-2,-2),(2,3),(-2,1),(2,0),(-3,1)] [(0,0)]
