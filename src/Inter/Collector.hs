module Inter.Collector where

import Inter.Types

import qualified NFA.Analyse
import qualified NFA.Synthese
import qualified PCProblem.Quiz
import qualified Boolean.Instance
import qualified Boolean.Quiz

makers :: [ Make ]
makers = [ NFA.Analyse.make
	 , NFA.Synthese.make
	 , PCProblem.Quiz.make_quiz
	 , PCProblem.Quiz.make_fixed
	 , Boolean.Instance.make
	 , Boolean.Quiz.make
	 ]
