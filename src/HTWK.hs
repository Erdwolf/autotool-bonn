module Inter.Boiler where

--   $Id$

import Inter.Types

import qualified Inter.CASE

-- hier steht ALLES machbare drin
-- in der datenbank steht dann, zu welcher zeit es erlaubt ist.

boiler :: IO [ Variant ]
boiler = sequence 
       -- der erste ist der default-wert!
       $  []
       ++ Inter.CASE.aufgaben
       


