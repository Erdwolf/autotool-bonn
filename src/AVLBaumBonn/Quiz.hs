{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances #-}
module AVLBaumBonn.Quiz where

import Inter.Quiz
import Inter.Types

import qualified Baum.Such.Config
import qualified Baum.Such.Generate

import AVLBaumBonn.Central

type QuizConfig = Baum.Such.Config.Config Int

instance Generator AVLBaum QuizConfig Config where
    generator _ conf _key = Baum.Such.Generate.generate conf

instance Project AVLBaum Config Config where
    project _ i = i

make :: Make
make = quiz AVLBaum Baum.Such.Config.example
