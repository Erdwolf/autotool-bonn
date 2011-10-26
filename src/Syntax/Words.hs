{-#  LANGUAGE TupleSections #-}
module Syntax.Words where

import Syntax.Syntax
import Syntax.Checker
import Syntax.Transformer

import Control.Monad
import Control.Monad.Writer
import Data.List (nubBy)
import Data.Function (on)


type Word = [Item]
type Symbols = [String]

generateWords :: Int -> -- ^ 'maxSteps" is the maximal number of production steps, not the number
                        -- ^ of generated words, although these are sometimes the same.
                 Language ->
                 [(Word,Symbols)] -- ^ Words and Symbols used to generate them.
generateWords maxSteps lang =
    filter (all terminal . fst) $
    iterate oneStep [([startSymbol],[])] !! maxSteps
 where
    terminal (T _) = True
    terminal _     = False

    oneStep ws = nubBy ((==)`on`fst) $ ws ++ (ws >>= produceNew)

    --produceNew (w,ss) = runWriterT $ tell ss >> liftM concat (mapM replace w)
    produceNew (w,ss) = runWriterT $ tell ss >> partitions w >>= f

    f (as,b:bs) = ((as++).(++bs)) `liftM` replace b

    replace (N nt) = tell [nt] >> lookupAll nt productions
    replace x = return [x]

    startSymbol = N $ fst $ head productions

    productions = map (\(Rule x y) -> (x,y)) $ toGrammar lang

    toGrammar = map toRule . removeForks . removeLoops

partitions (x:xs) = return ([],x:xs) `mplus` liftM (\(as,bs) -> (x:as,bs)) (partitions xs)
partitions []     = mzero
