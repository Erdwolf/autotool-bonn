-- | Autotool Challenger Partial
module Challenger.Partial where

--   $Id$

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

-- | Klasse: Partial
class ( ToDoc p, ToDoc i, Reader b, ToDoc b , Size b )
    => Partial p i b | p i -> b , p b -> i where

      -- | Beschreibung der Aufgabe herstellen
      --
      -- TODO: es sollte (auch oder nur) eine Beschreibung geben,
      -- die nur von p allein abh�ngt (dann mu� man nicht erst erzeugen
      -- und kann trotzdem schon was ausgeben)
      describe :: p -> i -> Doc

      -- | falls wir auch rechnen wollen (z. b. Bilder malen)
      -- hat default-implementierung, die describe benutzt
      report   :: p -> i -> Reporter ()
      report p i = inform $ describe p i -- default

      -- | ein sinnvoller startpunkt f�r die l�sung
      initial :: p -> i -> b

      -- | pr�fe, ob l�sung partiell korrekt
      -- d. h. l��t sich zu total korrekter erweitern
      -- hat defaul-imp, die immer ja sagt
      partial :: p -> i -> b -> Reporter ()
      -- default
      partial p i b = return ()

      -- | hier k�nnen wir irgendwas vorrechen
      -- nachdem der partial-test bestanden wurde 
      -- (zur Reihenfolge siehe Inter.Evaluate)
      demonstrate :: p -> i -> b -> Reporter ()
      demonstrate p i n = return ()

      -- | alles richtig?
      -- vorher wird immer erst partial angewendet
      total   :: p -> i -> b -> Reporter ()

      -- | bewertung der l�sung
      measure :: p -> i -> b -> Int
      -- | default:
      measure p i b = size b
      
