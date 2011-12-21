{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances #-}
module TypeCheckBonn.Quiz where

--  $Id$

import Type.Data
import TypeCheckBonn.Central
import Type.Quiz
import Type.Check

import Inter.Quiz
import Inter.Types

import Autolib.TES.Identifier


bonnify :: TI -> TI
bonnify (TI t (Signature fs vs)) = TI t (Signature (map discharge fs) vs)
  where
    discharge f = f { static = False }


instance Generator TypeCheckBonn Conf InstanceConf where
    generator p conf key = do
        generator TypeCheck conf key

instance Project TypeCheckBonn InstanceConf TI where
    project p = bonnify . project TypeCheck

{-
instance Generator TypeCheckBonn QuizConfig Config where
    generator p (QuizConfig fb a ts a b m n _) key = do
        TI t s <- generator TypeCheck (Conf a (map read ts) a b m n) key
        return $ Config fb t (bonnify s)

instance Project TypeCheckBonn Config Config where
    project p = id
-}


make :: Make
make = quiz TypeCheckBonn $
         Conf { max_arity = 3
              , types = read "[ int, double, char, Baum, Person ]"
              , min_symbols = 4
              , max_symbols = 10
              , min_size = 5
              , max_size = 10
              }
{-
         QuizConfig
              { quizFeedback = ???
              , max_arity = 3
              , types = [ "int", "double", "char", "Baum", "Person" ]
              , minDeclss = 4
              , maxDecls = 10
              , minSize = 5
              , maxSize = 10
              }
-}
