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


bonnify :: TI Identifier -> TI IdentifierBonn
bonnify (TI t (Signature fs vs)) = TI t (Signature (map g $ map discharge fs) (map h vs))
  where
    discharge f = f { static = False }
    g f = f { fname = IdentifierBonn (fname f) }
    h v = v { vname = IdentifierBonn (fname f) }


instance Generator TypeCheckBonn Conf InstanceConf where
    generator p conf key = do
        generator TypeCheck conf key


instance Project TypeCheckBonn InstanceConf (TI IdentifierBonn) where
    project p = bonnify . project TypeCheck

make :: Make
make = quiz TypeCheckBonn $
         Conf { max_arity = 3
              , types = read "[ int, double, char, Baum, Person ]"
              , min_symbols = 4
              , max_symbols = 10
              , min_size = 5
              , max_size = 10
              }
