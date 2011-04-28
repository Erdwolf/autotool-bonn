{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Type.Poly.Quiz where

import Type.Poly.Data
import Type.Poly.Roll
import Type.Poly.Check ( TypePolyCheck (..) )

import Inter.Quiz
import Inter.Types

instance Generator TypePolyCheck Conf ( TI, Expression ) where
    generator p conf key = roller conf

instance Project TypePolyCheck ( TI, Expression ) TI where
    project p ( ti, x ) = ti

make :: Make
make = quiz TypePolyCheck conf



