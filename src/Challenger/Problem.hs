module Challenger.Problem where

--   $Id$


import Reporter
import ToDoc
import System (ExitCode)

-- | Klasse Problem steht für ein abstraktes Problem zu dem

-- Instanzen mit Beweisen existieren und auf Korrektheit 
-- ueberprueft werden koennen
--  validiere ist zum vorpruefen der Instanz gedacht
--  verifiziere dann zum ueberpruefen der Loesung (des Beweises)
class (Show p, ToDoc i, Show i, Read i
  , ToDoc b, Show b, Read b)
    => Problem  p i b | p b -> i  where

       validiere   :: p -> i -> b -> (Bool, Doc) 
       verifiziere :: p -> i -> b -> (Bool, Doc) 

       -- für den gleitenden wechsel zur reporter-monade:
       -- die obigen methoden deprecated, die neuen sind:
       validiereR  :: p -> i -> b -> Reporter ()
       verifiziereR  :: p -> i -> b -> Reporter ()

       -- default-instanzen zum umschalten
       validiere p i b = lazy_cheporter $ validiereR p i b
       verifiziere p i b = lazy_cheporter $ verifiziereR p i b

       -- rückwarts-defaults
       validiereR p i b = porterche $ validiere p i b
       verifiziereR p i b = porterche $ verifiziere p i b

       -- String ist der Dateiname ohne Endung 
       -- return: datei, Typ (endung), ExitCode des Systemaufrufs
       getInstanz :: p -> i -> b -> String -> IO(String, String, ExitCode)
       getBeweis  :: p -> i -> b -> String -> IO(String, String, ExitCode)

  
