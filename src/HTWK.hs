module Inter.Boiler where

--   $Id$

import Inter.Types

import qualified Inter.Compilerbau
import qualified Inter.Informatik
import qualified Inter.Grundlagen

-- hier steht ALLES machbare drin
-- in der datenbank steht dann, zu welcher zeit es erlaubt ist.

boiler :: IO [ Variant ]
boiler = sequence 
       -- der erste ist der default-wert!
       $  Inter.Grundlagen.aufgaben
       ++ Inter.Compilerbau.aufgaben
       ++ Inter.Informatik.aufgaben

