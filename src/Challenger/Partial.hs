-- | Autotool Challenger Partial
module Challenger.Partial where

--   $Id$

import Reporter
import ToDoc

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

      -- | alles richtig?
      -- vorher wird immer erst partial angewendet
      total   :: p -> i -> b -> Reporter ()
