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

import qualified Syntax.Synthese
import qualified Syntax.Analyse

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

          Syntax.Synthese.generates
       ++ Syntax.Analyse.generates

       ++ Sortier.Netz.Check.generates 
       ++ PCP.Quiz.generates
       ++ Hanoi.generates

       ++ map (fmap Variant) 
       [ JVM.TIMES.generate
       , JVM.EXP1.generate
       , JVM.EXP2.generate

       , JVM.NUM32.generate
       , JVM.NUM42.generate
       , JVM.NUMAT.generate
       ]


	++ Java.Sort.generates
