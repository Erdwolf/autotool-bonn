module Inter.Collector where

import Inter.Types

import qualified Blank
import qualified NFA.Analyse
import qualified NFA.Synthese
import qualified PCProblem.Quiz
import qualified Boolean.Instance
import qualified Boolean.Quiz
import qualified Sortier.Netz.Check 
import qualified JVM.Make
import qualified Graph.Selfcom
import qualified Graph.Nachbar
import qualified Graph.Cross
import qualified Robots.Interface
import qualified Graph.Col.Plain
import qualified Graph.Col.Quiz
import qualified Graph.Cage.Central
import qualified Graph.Graceful.Central
import qualified Collatz.Plain
import qualified Collatz.Inverse
import qualified Hanoi.Semantik
import qualified Hanoi.Quiz

makers :: [ Make ]
makers = [ Blank.make
	 , NFA.Analyse.make
	 , NFA.Synthese.make
	 , PCProblem.Quiz.make_quiz
	 , PCProblem.Quiz.make_fixed
	 , Boolean.Instance.make
	 , Boolean.Quiz.make
	 , Sortier.Netz.Check.make
	 , JVM.Make.make
	 , Graph.Selfcom.make
	 , Graph.Nachbar.make
	 , Graph.Cross.make
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
	 ]

