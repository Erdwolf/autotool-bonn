module Inter.Boiler where

-- $Id$

import Inter.Types


import qualified Syntax.Analyse
import qualified Syntax.Synthese

-- hier steht ALLES machbare drin
-- in der datenbank steht dann, zu welcher zeit es erlaubt ist.

boiler :: IO [ Variant ]
boiler = sequence -- der erste ist der default-wert!       
       $  Syntax.Analyse.generates
       ++ Syntax.Synthese.generates



