-- | Autotool Challenger Partial
module Challenger.Partial where

--   $Id$

import Reporter
import ToDoc
import Size

-- | Klasse: Partial
class Partial p i b | p i -> b , p b -> i where

      -- | Beschreibung der Aufgabe herstellen
      --
      -- TODO: es sollte (auch oder nur) eine Beschreibung geben,
      -- die nur von p allein abhängt (dann muß man nicht erst erzeugen
      -- und kann trotzdem schon was ausgeben)
      describe :: p -> i -> Doc

      -- | falls wir auch rechnen wollen (z. b. Bilder malen)
      -- hat default-implementierung, die describe benutzt
      report   :: p -> i -> Reporter ()
      report p i = inform $ describe p i -- default

      -- | ein sinnvoller startpunkt für die lösung
      initial :: p -> i -> b

      -- | prüfe, ob lösung partiell korrekt
      -- d. h. läßt sich zu total korrekter erweitern
      -- hat defaul-imp, die immer ja sagt
      partial :: p -> i -> b -> Reporter ()
      -- default
      partial p i b = return ()

      -- | hier können wir irgendwas vorrechen
      -- nachdem der partial-test bestanden wurde 
      -- (zur Reihenfolge siehe Inter.Evaluate)
      demonstrate :: p -> i -> b -> Reporter ()
      demonstrate p i n = return ()

      -- | alles richtig?
      -- vorher wird immer erst partial angewendet
      total   :: p -> i -> b -> Reporter ()


class Measure p i b where
      -- | bewertung der lösung
      measure :: p -> i -> b -> Int
      
-- | default instance 
-- kann überschrieben werden durch genaue Angaben von p, b
instance Size b => Measure p i b where
      measure p i b = size b