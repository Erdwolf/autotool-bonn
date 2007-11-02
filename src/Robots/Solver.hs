{-# OPTIONS -fglasgow-exts #-}

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


nachfolger_without_loss :: Config -> [ Config ]
nachfolger_without_loss k = do
    n <- nachfolger k
    guard $ length ( robots n ) == length ( robots k )
    return n

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
    guard $ not $ empty_quads k' -- ??
    return ( z, k' )

znachfolger_all_onboard k = do
    let ( t, _ :: Doc ) = export $ valid k
    guard $ isJust t
    r <- robots k
    d <- richtungen
    let z = (name r, d)
    k' <- maybeToList $ execute k z
    guard $ covered k'
    guard $ not $ empty_quads k' -- ??
    guard $ length ( robots k ) == length ( robots k' )
    return ( z, k' )


empty_quads k = 
    case ( do r <- robots k ; maybeToList $ ziel r ) of
        [ (x0,y0) ] -> or $ do
            let comps = [ (<=), (>=) ]
            gx <- comps
            gy <- comps
            return $ null $ do
                r <- robots k
                let (x,y) = position r
                guard $ gx x x0 && gy y y0
                return ()
        _ -> False -- DON'T KNOW


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
ex = Robots.Config.make 
     [ Robot { name = "A", position = ( -2, -2 ) , ziel = Nothing }
       , Robot { name = "B", position = ( 2, 3 ), ziel = Just ( 0, 0 ) }
       , Robot { name = "C", position = ( -2, 1 ), ziel = Nothing }
       , Robot { name = "D", position = ( 2, 0 ), ziel = Nothing }
       , Robot { name = "E", position = ( -3, 1 ), ziel = Nothing }
       ]