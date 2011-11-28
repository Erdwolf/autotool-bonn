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
    generator p (QuizConfig fb) key = do -- IO
      Config fb <$> HeapSort.Generator.generate


instance Project HeapSort Config Config where
    project p = id



make :: Make
make = quiz HeapSort (QuizConfig True)
