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

solutions :: Konfig -> [ Doc ]
solutions k = do
    (d, ps) <- zip [0 :: Int ..] $ schichten ( mkSet . nachfolger ) k
    let out = do p <- setToList ps
		 return $ if fst $ final p 
		    then geschichte p
		    else []
    return $ toDoc (( d, length out), out)

main :: IO ()
main = sequence_ $ map print $ solutions ex


