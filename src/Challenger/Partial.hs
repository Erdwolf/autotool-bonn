-- | Autotool Challenger Partial
module Challenger.Partial where

--   $Id$

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

class Measure p i b where
      measure :: p -> i -> b -> Integer

-- | die Messung der Größe ist von der Prüfung der Lösung getrennt.
-- das ist schlecht, falls dadurch aufwendige Rechnungen
-- wiederholt werden müssen (siehe z. B. Graph.Cross)
instance Size b => Measure p i b where
      measure _ _ b = fromIntegral $ size b
      
-- | Klasse: Partial
class ( ToDoc p, ToDoc i, Reader b, ToDoc b, Measure p i b )
    => Partial p i b | p i -> b , p b -> i where
      -- | Beschreibung der Aufgabe herstellen
      --
      -- TODO: es sollte (auch oder nur) eine Beschreibung geben,
      -- die nur von p allein abhängt (dann muß man nicht erst erzeugen
      -- und kann trotzdem schon was ausgeben)
      describe :: p -> i -> Doc
      describe p i = vcat
          [ text "Problem" <+> toDoc p
	  , text "Instanz" <+> toDoc i
	  ]

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

      -- | hier können wir irgendwas vorrechen,
      -- nachdem der partial-test bestanden wurde 
      -- (zur Reihenfolge siehe Inter.Evaluate)
      demonstrate :: p -> i -> b -> Reporter ()
      demonstrate p i n = return ()

      -- | alles richtig?
      -- vorher wird immer erst partial angewendet
      total   :: p -> i -> b -> Reporter ()
