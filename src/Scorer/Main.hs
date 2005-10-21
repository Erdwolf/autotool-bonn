-- module Main where

import qualified Scorer.Aufgabe
import Scorer.Compute
import Scorer.Util

import qualified Control.Schule as U
import qualified Control.Vorlesung as V

import Autolib.FiniteMap
import Control.Monad ( guard, when )
import Data.Maybe

-- | usage: ScorerDB file1 file2 ...

main :: IO ()
main = do
    t <- zeit    
    putStrLn $ heading $ "autotool -- Top Ten Scores, Stand von: " ++ t
    vls <- Scorer.Aufgabe.get
    schulen <- U.get
    sequence_ $ do
        schule <- schulen
        return $ do
            vors <- V.get_at_school $ U.unr schule 
            let table = do
                vor <- vors
                info <- maybeToList $ lookupFM vls ( V.vnr vor )
                return ( vor, info )
            mapM_ ( compute schule ) table


