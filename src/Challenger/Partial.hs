-- | Autotool Challenger Partial
module Challenger.Partial 

( Wert (..)
, Measure (..)
, Partial (..)
)

where

--   $Id$

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Control.Types ( Wert (..) )

class Measure p i b where
      measure :: p -> i -> b -> Integer

-- | die Messung der Gr��e ist von der Pr�fung der L�sung getrennt.
-- das ist schlecht, falls dadurch aufwendige Rechnungen
-- wiederholt werden m�ssen (siehe z. B. Graph.Cross)
instance Size b => Measure p i b where
      measure _ _ b = fromIntegral $ size b
      
-- | Klasse: Partial
class ( ToDoc p, ToDoc i, Reader b, ToDoc b, Measure p i b )
    => Partial p i b | p i -> b , p b -> i where
      -- | Beschreibung der Aufgabe herstellen
      --
      -- TODO: es sollte (auch oder nur) eine Beschreibung geben,
      -- die nur von p allein abh�ngt (dann mu� man nicht erst erzeugen
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

      -- | ein sinnvoller startpunkt f�r die l�sung
      initial :: p -> i -> b

      -- | pr�fe, ob l�sung partiell korrekt
      -- d. h. l��t sich zu total korrekter erweitern
      -- hat defaul-imp, die immer ja sagt
      partial :: p -> i -> b -> Reporter ()
      -- default
      partial p i b = return ()

      -- | hier k�nnen wir irgendwas vorrechen,
      -- nachdem der partial-test bestanden wurde 
      -- (zur Reihenfolge siehe Inter.Evaluate)
      demonstrate :: p -> i -> b -> Reporter ()
      demonstrate p i n = return ()

      -- | alles richtig?
      -- vorher wird immer erst partial angewendet
      -- eine der beiden total, total_neu mu� implementiert werden
      total   :: p -> i -> b -> Reporter ()
      total p i b = do
          res <- total_neu p i b
          case res of
	      Ok _ -> return ()
              _    -> reject $ toDoc res

      -- | liefert (in jedem Fall) einen Wert
      total_neu :: p -> i -> b -> Reporter Wert
      total_neu p i b = do
          mres <- wrap $ total p i b
	  return $ case mres of
	       Nothing -> No
	       Just () -> Ok $ measure p i b
 