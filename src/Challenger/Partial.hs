module Challenger.Partial where

-- $Id$

import Reporter

class Partial p i b | p i -> b , p b -> i where
      -- ein sinnvoller startpunkt f�r die l�sung
      initial :: p -> i -> b

      -- pr�fe, ob l�sung partiell korrekt
      -- d. h. l��t sich zu total korrekter erweitern
      partial :: p -> i -> b -> Reporter ()
      -- default
      partial p i b = return ()

      -- alles richtig?
      total   :: p -> i -> b -> Reporter ()
