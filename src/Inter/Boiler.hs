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

import qualified PCP.QUIZ
import qualified SAT.QUIZ

import qualified JVM.TIMES
import qualified JVM.EXP1
import qualified JVM.EXP2

import qualified JVM.NUM32
import qualified JVM.NUM42
import qualified JVM.NUMMAT

import qualified Demo.L

-- hier steht ALLES machbare drin
-- in der datenbank steht dann, zu welcher zeit es erlaubt ist.

boiler :: IO [ Variant ]
boiler = sequence $
       -- der erste ist der default-wert!
       [ fmap Variant $ JVM.TIMES.generate
       , fmap Variant $ JVM.EXP1.generate
       , fmap Variant $ JVM.EXP2.generate

       , fmap Variant $ PCP.QUIZ.generate
       , fmap Variant $ SAT.QUIZ.generate

       , fmap Variant $ TM.FACTOR.generate

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

