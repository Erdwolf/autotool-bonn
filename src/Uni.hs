module Inter.Boiler where

--   $Id$

import Inter.Types


-- import qualified Syntax.Analyse
-- import qualified Syntax.Synthese
-- import qualified Syntax.Grammatik
-- import qualified Syntax.Pumping

-- import Serie6

import qualified Uni.SS04.Serie1
import qualified Uni.SS04.Serie2

-- hier steht ALLES machbare drin
-- in der datenbank steht dann, zu welcher zeit es erlaubt ist.

boiler :: IO [ Variant ]
boiler = sequence -- der erste ist der default-wert!       
       $  []

       ++ Uni.SS04.Serie1.generate
       ++ Uni.SS04.Serie2.generate




