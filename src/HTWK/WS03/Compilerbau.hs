module Inter.Compilerbau where

--   $Id$

-- TODO: directories aufräumen:
-- jetzt haben wir LV-spezifische teile
-- sowohl in htwk/Inter/* als auch in ws03/compilerbau/* usw.

-- ws03/compilerbau:


import qualified Syntax.Keller
import qualified Syntax.Grammatik

import qualified Syntax.Equiv

import qualified Syntax.Synthese
import qualified Syntax.Determine
import qualified Syntax.Analyse


import qualified JVM.TIMES
import qualified JVM.EXP1
import qualified JVM.EXP2

import qualified JVM.NUM32
import qualified JVM.NUM42
import qualified JVM.NUMAT


import Inter.Types 

aufgaben = []

       ++ Syntax.Synthese.generates
       ++ Syntax.Analyse.generates
       ++ Syntax.Grammatik.generates         


       ++ Syntax.Keller.generates

       ++ Syntax.Equiv.generates
       ++ Syntax.Determine.generates

       ++ map (fmap Variant) 
       [ JVM.TIMES.generate
       , JVM.EXP1.generate
       , JVM.EXP2.generate

       , JVM.NUM32.generate
       , JVM.NUM42.generate
       , JVM.NUMAT.generate
       ]

