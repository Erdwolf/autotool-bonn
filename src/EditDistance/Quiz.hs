{-# LANGUAGE MultiParamTypeClasses #-}
module EditDistance.Quiz where

import Control.Applicative

import Inter.Quiz
import Inter.Types

import EditDistance.Central
import EditDistance.Data
import EditDistance.Generator

import Data.Typeable



instance Generator EditDistance QuizConfig Config where
    generator p (QuizConfig fb e n tgen a) key = do -- IO
      uncurry (Config fb e) <$> case tgen of
                                   Length m ->
                                      EditDistance.Generator.generate  n m a
                                   InsertReplaceDelete d r i ->
                                      EditDistance.Generator.generate2 n (d,r,i) a


instance Project EditDistance Config Config where
    project p = id



make :: Make
make = quiz EditDistance (QuizConfig NumberOfErrors 0 5 (InsertReplaceDelete 3 3 3) 4)
