module Robots.Solver where

import Robots.Type
import Robots.Hull

import Schichten

import Maybe
import Set
import ToDoc
import Monad ( guard )


nachfolger :: Konfig -> [ Konfig ]
nachfolger k = do
    guard $ fst $ valid k
    r <- robots k
    d <- richtungen
    let z = (name r, d)
    k' <- maybeToList $ execute k z
    guard $ covered k'
    return k'

solutions :: Konfig -> [ ((Int, Int), [[Zug]]) ]
solutions k = do
    (d, ps) <- zip [0 :: Int ..] $ schichten ( mkSet . nachfolger ) k
    let out = do p <- setToList ps
		 return $ if fst $ final p 
		    then reverse $ geschichte p
		    else []
    return $ (( d, length out), filter (not . null) out)

solve :: Konfig -> IO ()
solve k = sequence_ $ map ( print . toDoc ) $ solutions k

shortest :: Konfig -> [[Zug]]
shortest k = take 1 $ do
    ( _ , zss ) <- solutions k
    zss
