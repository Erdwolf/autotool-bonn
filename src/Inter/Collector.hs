module Inter.Collector where

import Inter.Types

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

makers :: [ Make ]
makers = [ NFA.Analyse.make
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
	 ]

