module Inter.Boiler where

-- $Id$

import Inter.Types


import qualified Syntax.Analyse
import qualified Syntax.Synthese
import qualified Syntax.Grammatik
import qualified Syntax.Pumping

-- hier steht ALLES machbare drin
-- in der datenbank steht dann, zu welcher zeit es erlaubt ist.

boiler :: IO [ Variant ]
boiler = sequence -- der erste ist der default-wert!       
       $  []
	  -- Serie 5 (?)
       ++ Syntax.Grammatik.grammatiken5
	  -- Serie 4:
       ++ Syntax.Grammatik.grammatiken
       ++ Syntax.Pumping.pumping
	  -- Serie (davor)
       ++ Syntax.Analyse.generates
       ++ Syntax.Synthese.generates




