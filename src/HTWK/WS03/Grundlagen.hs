module Inter.Grundlagen where

--   $Id$


-- ws03/grundlagen:

import qualified Syntax.Synthese
import qualified Syntax.Analyse
import qualified Syntax.Grammatik

aufgaben =  Syntax.Analyse.generates
	 ++ Syntax.Synthese.generates
	 ++ Syntax.Grammatik.generates

