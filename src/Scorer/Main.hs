-- module Main where

import qualified Scorer.Aufgabe
import Scorer.Compute
import Scorer.Util

import Data.FiniteMap

-- | usage: ScorerDB file1 file2 ...

main :: IO ()
main = do
    t <- zeit    
    putStrLn $ heading $ "autotool -- Top Ten Scores, Stand von: " ++ t

    vls <- Scorer.Aufgabe.get

    -- print vls -- funktioniert, head [] kommt später

    mapM_ compute $ fmToList vls


