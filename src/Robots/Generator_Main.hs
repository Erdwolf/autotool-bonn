module Main (main) where

import Robots.Generator
import System
import IORef

main :: IO ()
main = do
    [ n, w ] <- getArgs
    top <- newIORef 0
    sequence_ $ repeat $ action top ( read n ) ( read w )

