module Robots.Solver where

--  $Id$

import Robots.Config
import Robots.Data
import Robots.Move
import Robots.Interface
import Robots.Hull
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

solve :: Config -> IO ()
solve k = sequence_ $ map ( print . toDoc ) $ solutions k

shortest :: Config -> [[Zug]]
shortest k = take 1 $ do
    ( _ , zss ) <- solutions k
    zss
