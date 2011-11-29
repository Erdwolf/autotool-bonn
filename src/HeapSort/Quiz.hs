{-# LANGUAGE MultiParamTypeClasses #-}
module HeapSort.Quiz where

import Control.Applicative

import Inter.Quiz
import Inter.Types

import HeapSort.Central
import HeapSort.Tree
import HeapSort.Data
import HeapSort.Generator

import Data.Typeable



instance Generator HeapSort QuizConfig Config where
    generator p (QuizConfig fb k n min max) key = do -- IO
      Config fb <$> HeapSort.Generator.generate k n (min,max)


instance Project HeapSort Config Config where
    project p = id



make :: Make
make = quiz HeapSort (QuizConfig OnFailure 3 7 1 100)
