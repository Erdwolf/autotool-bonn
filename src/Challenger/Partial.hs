module Challenger.Partial where

-- $Id$

import Reporter

class Partial p i b | p i -> b , p b -> i where
      -- ein sinnvoller startpunkt für die lösung
      initial :: p -> i -> b

      -- prüfe, ob lösung partiell korrekt
      -- d. h. läßt sich zu total korrekter erweitern
      partial :: p -> i -> b -> Reporter ()
      -- default
      partial p i b = return ()

      -- alles richtig?
      total   :: p -> i -> b -> Reporter ()
