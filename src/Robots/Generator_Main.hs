module Main (main) where

import Robots.Generator
import System

main :: IO ()
main = do
    [ n, w ] <- getArgs
    sequence_ $ repeat $ action ( read n ) ( read w )

