{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

-- | Autotool Challenger Partial
module Challenger.Partial 

( Wert (..)
, Measure (..)
, Verify (..)
, Partial (..)
, Roller (..)
)

where

--   $Id$

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size

import Control.Types ( Wert (..), ok )

class Measure p i b where
      measure :: p -> i -> b -> Integer

-- | die Messung der Größe ist von der Prüfung der Lösung getrennt.
-- das ist schlecht, falls dadurch aufwendige Rechnungen
-- wiederholt werden müssen (siehe z. B. Graph.Cross)
instance Size b => Measure p i b where
      measure _ _ b = fromIntegral $ Autolib.Size.size b
      

-- | Instanz verifizieren 
-- das sieht der Tutor, wenn er die Aufgabe konfiguriert
class Verify p i where
      verify :: p -> i -> Reporter ()

instance Verify p i where
      verify p i = return () -- default: alles OK

-- | Klasse: Partial
class ( Show p, Read p
      , ToDoc i, Read i -- Modular/Server muß auch Instanzen lesen
      , Reader b, ToDoc b
      , Measure p i b 
      )
    => Partial p i b | p i -> b  where

      -- | Beschreibung der Aufgabe herstellen
      --
      -- TODO: es sollte (auch oder nur) eine Beschreibung geben,
      -- die nur von p allein abhängt (dann muß man nicht erst erzeugen
      -- und kann trotzdem schon was ausgeben)
      describe :: p -> i -> Doc
      describe p i = vcat
          [ text "Problem" <+> text ( show p )
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
      -- eine der beiden total, total_neu muß implementiert werden
      total   :: p -> i -> b -> Reporter ()
      total p i b = do
          res <- total_neu p i b
          case res of
	      Okay {} -> return ()
              _    -> reject $ toDoc res

      -- | liefert (in jedem Fall) einen Wert
      total_neu :: p -> i -> b -> Reporter Wert
      total_neu p i b = do
          mres <- wrap $ total p i b
	  return $ case mres of
	       Nothing -> No
	       Just () -> ok $ measure p i b

---------------------------------------------------------------------

-- | falls man erstmal etwas ausrechnen (z. b. würfeln) will
class Roller i i' | i -> i' where
      roller :: i -> IO i'

instance Roller i i where
      roller = return


 