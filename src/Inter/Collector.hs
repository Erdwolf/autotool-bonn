module Inter.Collector where

import Inter.Types

import qualified Blank
import qualified Upload

import qualified PCProblem.Quiz
import qualified Boolean.Instance
import qualified Boolean.Quiz
import qualified Sortier.Netz.Check 
import qualified JVM.Make
import qualified Graph.Selfcom
import qualified Graph.Nachbar
import qualified Graph.Cross
import qualified Graph.MinSep
import qualified Robots.Interface
import qualified Graph.Col.Plain
import qualified Graph.Col.Quiz
import qualified Graph.Cage.Central
import qualified Graph.Graceful.Central
import qualified Collatz.Plain
import qualified Collatz.Inverse
import qualified Hanoi.Semantik
import qualified Hanoi.Quiz
import qualified Type.Check
import qualified Type.Quiz
-- import qualified Object.Check
-- import qualified Object.Quiz
import qualified Palindrom.Plain

import qualified Faktor.Faktor

import qualified NFA.Convert
import qualified NFA.Equiv.Challenger
import qualified Exp.Convert
import qualified Grammatik.CF.Interface
import qualified NPDA.Inter
import qualified SAT.SAT
import qualified Baum.Reconstruct
import qualified Baum.Binary
import qualified Baum.ZweiDrei
import qualified Graph.TreeWidth
import qualified Graph.PartialKTree

import qualified Graph.Bi.Quiz
import qualified Graph.Bi.Plain
import qualified Graph.Circle.Quiz
import qualified Graph.Circle.Plain

makers :: [ Make ]
makers = [ Blank.make
	 , Upload.make
	 , PCProblem.Quiz.make_quiz
	 , PCProblem.Quiz.make_fixed
	 , Boolean.Instance.make
	 , Boolean.Quiz.make
	 , Sortier.Netz.Check.make
	 , JVM.Make.make
	 , Graph.Selfcom.make
	 , Graph.Nachbar.make
	 , Graph.Cross.make
	 , Graph.MinSep.make
	 , Robots.Interface.make
	 , Robots.Interface.qmake
	 , Graph.Col.Plain.make
	 , Graph.Col.Quiz.make
	 , Graph.Cage.Central.make
	 , Graph.Graceful.Central.make
	 , Collatz.Plain.make
	 , Collatz.Plain.qmake
	 , Collatz.Inverse.make
	 , Collatz.Inverse.qmake
	 , Hanoi.Semantik.make
	 , Hanoi.Quiz.make
	 , Type.Check.make
	 , Type.Quiz.make
	 , Palindrom.Plain.make

	 , NFA.Convert.make
	 , NFA.Convert.qmake
	 , Exp.Convert.make
	 , Exp.Convert.qmake
	 , NFA.Equiv.Challenger.make
	 , NFA.Equiv.Challenger.qmake

         , Grammatik.CF.Interface.make
	 , NPDA.Inter.make

         , SAT.SAT.make_fixed
         , SAT.SAT.make_quiz

         , Baum.Reconstruct.make_fixed
         , Baum.Reconstruct.make_quiz
         , Baum.Binary.make_quiz
         , Baum.ZweiDrei.make_quiz

         , Graph.TreeWidth.make
         , Graph.PartialKTree.make
         , Graph.PartialKTree.qmake

         , Faktor.Faktor.make_fixed
         , Faktor.Faktor.make_quiz

	 , Graph.Bi.Quiz.make
	 , Graph.Bi.Plain.make
	 , Graph.Circle.Quiz.make
	 , Graph.Circle.Plain.make
	 ]
