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
    r <- robots k
    d <- richtungen
    let z = (name r, d)
    k' <- maybeToList $ execute k z
    guard $ covered k'
    return k'

solutions :: Konfig -> [[[Zug ]]]
solutions k = do
    ps <- schichten ( mkSet . nachfolger ) k
    return $ do p <- setToList ps
		if fst $ final p 
		   then return $ geschichte p
		   else return []

main :: IO ()
main = sequence_ $ map print $ solutions ex


