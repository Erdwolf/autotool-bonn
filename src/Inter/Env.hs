module Inter.Env where

-- $Id$

-- liest parameter-Werte aus CGI-Env

import qualified Inter.Param as P
import qualified Passwort

type Env = [(String, String)]

get :: Env  -> P.Type
-- default: leerer string
get env = get_with env P.example

get_with :: Env -> P.Type -> P.Type
get_with env def = 
    if null $ item env "matrikel"
    then def
    else P.empty { P.matrikel = item env "matrikel"
		   , P.passwort = read $ item env "passwort"
		   , P.problem  = item env "problem"
		   , P.aufgabe  = item env "aufgabe"
		   , P.version  = item env "version"
		   , P.input    = item env "input"
		   }

item :: Env -> String -> String
-- default: leerer string
item env key = 
    case lookup key env of
        Just val -> val
	Nothing   -> ""


