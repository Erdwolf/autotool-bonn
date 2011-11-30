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
    generator p (QuizConfig fb n m a) key = do -- IO
      uncurry (Config fb e) <$> EditDistance.Generator.generate n m a


instance Project EditDistance Config Config where
    project p = id



make :: Make
make = quiz EditDistance (QuizConfig NumberOfErrors 0 5 7 4)
