module Syntax.Words where

import Syntax.Checker
import Syntax.Transformer

import Control.Monad
import Data.List (nub)


-- 'maxSteps" is the maximal number of production steps, not the number
-- of generated words, although these are sometimes the same.
generateWords maxSteps lang =
    filter (all terminal) $ iterate oneStep [[startSymbol]] !! maxSteps
 where
    terminal (T _) = True
    terminal _     = False

    oneStep ws = nub $ ws ++ (ws >>= produceNew)

    produceNew = liftM concat . mapM replace

    replace (N t) = lookupAll t productions
    replace x = [[x]]

    startSymbol = N $ fst $ head productions

    productions = map (\(Rule x y) ->  (x,y)) $ toGrammar lang

    toGrammar = map toRule . removeForks . removeLoops

