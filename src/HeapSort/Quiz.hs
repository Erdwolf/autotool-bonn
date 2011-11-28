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
    generator p (QuizConfig fb n min max) key = do -- IO
      Config fb <$> HeapSort.Generator.generate n (min,max)


instance Project HeapSort Config Config where
    project p = id



make :: Make
make = quiz HeapSort (QuizConfig OnFailure 7 1 100)
