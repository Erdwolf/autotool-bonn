module Challenger.Partial where

-- $Id$

import Reporter
import ToDoc

class Partial p i b | p i -> b , p b -> i where

      -- TODO: es sollte (auch oder nur) eine Beschreibung geben,
      -- die nur von p allein abhängt (dann muß man i nicht erst erzegen
      -- und kann trotzdem schon was ausgeben)
      describe :: p -> i -> Doc
      -- describe p i = "Partial.describe not implemented"

      -- ein sinnvoller startpunkt für die lösung
      initial :: p -> i -> b

      -- prüfe, ob lösung partiell korrekt
      -- d. h. läßt sich zu total korrekter erweitern
      partial :: p -> i -> b -> Reporter ()
      -- default
      partial p i b = return ()

      -- alles richtig?
      total   :: p -> i -> b -> Reporter ()
