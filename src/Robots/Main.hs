module Main (main) where

import Robots.Type
import Robots.Solver

import System

-- ein argument: filename
-- inhalt: konfiguration ( als " mkKonfig [ .. ] " )

main :: IO ()
main = do
     [ fname ] <- getArgs
     cs <- readFile fname
     let k = read cs :: Konfig
     print k
     solve k


     

