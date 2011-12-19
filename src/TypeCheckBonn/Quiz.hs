{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module TypeCheckBonn.Quiz where

--  $Id$

import Type.Data
import TypeCheckBonn.Central
import Type.Quiz

import Inter.Quiz
import Inter.Types


bonnify :: TI -> TI
bonnify TI (t sig) = TI t (map discharge sig)
  where
    discharge f = f { static = False }


instance Generator TypeCheckBonn Conf ( NFTA Int Type, TI ) where
    generator p conf key = do
        generator TypeCheck conf key


instance Project TypeCheckBonn ( NFTA Int Type, TI ) TI where
    project p ( au, ti ) = ti

make :: Make
make = quiz TypeCheckBonn conf
