module Inter.Boiler where

-- $Id$

import Inter.Types

import qualified TM.FACTOR

import qualified LOOP.SQRT
import qualified LOOP.TIMES
import qualified LOOP.PRIM
import qualified LOOP.FIB

import qualified FUN.SQRT
import qualified FUN.TIMES
-- import qualified FUN.PRIM
import qualified FUN.FIB
import qualified FUN.QUIZ

import qualified Demo.L

-- hier steht ALLES machbare drin
-- in der datenbank steht dann, zu welcher zeit es erlaubt ist.

boiler :: IO [ Variant ]
boiler = sequence $
       [ fmap Variant $ TM.FACTOR.generate

       , fmap Variant $ LOOP.SQRT.generate
       , fmap Variant $ LOOP.TIMES.generate
       , fmap Variant $ LOOP.PRIM.generate
       , fmap Variant $ LOOP.FIB.generate

       , fmap Variant $ FUN.SQRT.generate
       , fmap Variant $ FUN.TIMES.generate
       -- , fmap Variant $ FUN.PRIM.generate
       , fmap Variant $ FUN.FIB.generate
       , fmap Variant $ FUN.QUIZ.generate

       ]
       ++ Demo.L.generates

