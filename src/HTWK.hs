module Inter.Boiler where

--   $Id$

import Inter.Types
import HTWK.SS04


-- hier steht ALLES machbare drin
-- in der datenbank steht dann, zu welcher zeit es erlaubt ist.

boiler :: IO [ Variant ]
boiler = sequence 
       $  HTWK.SS04.aufgaben


