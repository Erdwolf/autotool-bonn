module Inter.Boiler where

-- $Id$

import Inter.Types


import qualified JVM.TIMES

-- ws03/compilerbau:

import qualified JVM.TIMES
import qualified JVM.EXP1
import qualified JVM.EXP2

import qualified JVM.NUM32
import qualified JVM.NUM42
import qualified JVM.NUMAT

-- ws03/informatik:
import qualified Sortier.Netz.Check
import qualified PCP.Quiz
import qualified Hanoi
import qualified Java.Sort

-- hier steht ALLES machbare drin
-- in der datenbank steht dann, zu welcher zeit es erlaubt ist.

boiler :: IO [ Variant ]
boiler = sequence $
       -- der erste ist der default-wert!

       [ 
           fmap Variant $ JVM.TIMES.generate
       , fmap Variant $ JVM.EXP1.generate
       , fmap Variant $ JVM.EXP2.generate

       , fmap Variant $ JVM.NUM32.generate
       , fmap Variant $ JVM.NUM42.generate
       , fmap Variant $ JVM.NUMAT.generate

       ]

       ++ Sortier.Netz.Check.generates 
       ++ PCP.Quiz.generates
       ++ Hanoi.generates

	++ Java.Sort.generates
