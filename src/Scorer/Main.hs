-- module Main where

import qualified Scorer.Aufgabe
import Scorer.Compute
import Scorer.Util

import qualified Control.Schule as U
import qualified Control.Vorlesung as V

import Autolib.ToDoc
import Autolib.Output ( Output )
import qualified Autolib.Output as O
import qualified Autolib.Multilingual
import qualified Text.XHtml

import Autolib.FiniteMap
import Control.Monad ( guard, when, forM )
import Data.Maybe

import System.Environment

-- | usage: ScorerDB [--cleartext] file1 file2 ...

main :: IO ()
main = do
    t <- zeit    
    let header = O.Text $ "autotool -- Top Ten Scores, Stand von: " ++ t
    vls <- Scorer.Aufgabe.get
    schulen <- U.get
    outss <- forM schulen $ \ schule -> do
            vors <- V.get_at_school $ U.unr schule 
            let table = do
                vor <- vors
                info <- maybeToList $ lookupFM vls ( V.vnr vor )
                return ( vor, info )
            forM table $ compute schule
    let out = O.lead header $ O.Itemize $ concat outss >>= maybeToList
    print $ Autolib.Multilingual.specialize Autolib.Multilingual.DE
          $ ( O.render out :: Autolib.Multilingual.Type Text.XHtml.Html )
    

