{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances #-}
module TypeCheckBonn.Quiz where

--  $Id$

import Type.Data hiding (types)
import TypeCheckBonn.Central
import TypeCheckBonn.Data
import Type.Quiz
import Type.Check

import Inter.Quiz
import Inter.Types

import Autolib.TES.Identifier


bonnify :: TI -> TI
bonnify (TI t (Signature fs vs)) = TI t (Signature (map discharge fs) vs)
  where
    discharge f = f { static = False }



instance Generator TypeCheckBonn QuizConfig Config where
    generator p (QuizConfig fb ar ts a b m n _) key = do
        cfg <- generator TypeCheck (Conf ar (map read ts) a b m n) key
        let TI t s = project cfg
        return $ Config fb t (bonnify s)

instance Project TypeCheckBonn Config Config where
    project p = id


make :: Make
make = quiz TypeCheckBonn $
         QuizConfig
              { quizFeedback = Detailed
              , max_arity = 3
              , types = [ "int", "double", "char", "Baum", "Person" ]
              , minDecls = 4
              , maxDecls = 10
              , minSize = 5
              , maxSize = 10
              }

foo :: Int
foo = "no int"
