module Inter.Collector where

import Inter.Types

import qualified NFA.Analyse
import qualified NFA.Synthese

makers :: [ Make ]
makers = [ NFA.Analyse.make
	 , NFA.Synthese.make
	 ]
