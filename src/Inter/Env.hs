module Inter.Env where

--   $Id$

-- liest parameter-Werte aus CGI-Env

import qualified Inter.Param as P
import qualified Control.Passwort

import Control.Types ( fromCGI )

import Inter.Click

import Control.Exception

type Env = [(String, String)]

get_with :: Env -> P.Type -> P.Type
get_with env def = 
    if or $ do it <- [ "Click", "Matrikel", "Passwort" ] 
	       return $ null $ item env it
    then def
    else def { P.mmatrikel = Just $ fromCGI $ item env "Matrikel" 
                   , P.mpasswort = Just $ read $ item env "Passwort"
                   -- , P.problem  = pav !! 0
                   -- , P.typ  = fromCGI $ pav !! 1
                   , P.aufgabe  = fromCGI $ item env "Aufgabe"
                   , P.input    = item env "Input"
		   , P.click    = read $ item env "Click"
                   }

item :: Env -> String -> String
-- default: leerer string
item env key = 
    case lookup key env of
        Just val  -> val
        Nothing   -> ""


