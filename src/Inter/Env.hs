module Inter.Env where

--   $Id$

-- liest parameter-Werte aus CGI-Env

import qualified Inter.Param as P
import qualified Control.Passwort

import Inter.Click

import Control.Exception

type Env = [(String, String)]

get :: Env  -> P.Type
-- default: leerer string
get env = get_with env P.example

get_with :: Env -> P.Type -> P.Type
get_with env def = 
    if null $ item env "Click"
     then def
     else 
        let	    
            -- String aufsplitten eg. "Computer:TM-2" -> "Computer TM 2"
            -- -> pav = ("Computer","TM","2") -- pav heisst problem,aufgabe,version
            mySub ':' = ' ' 
            mySub '-' = ' ' 
            mySub x@_   = x 
	    -- pav lesen aus 
	    pav = if item env "Click" == "Change"
		  -- eingabe boxen
	          then 
		  	if item env "Wahl" == "--"
			-- TODO: Hier keine Exception missbrauchen!
			then throw $ AssertionFailed "Keine Aufgabe gewaehlt!" 
			else words $ map mySub $ item env "Wahl" 
		  -- aus env-variablen
		  else map ( item env ) [ "Problem", "Aufgabe", "Version" ]
        in P.empty { P.matrikel = item env "Matrikel" 
                   , P.passwort = read $ item env "Passwort"
                   , P.problem  = pav !! 0
                   , P.aufgabe  = pav !! 1
                   , P.version  = pav !! 2
                   , P.input    = item env "Input"
		   , P.click    = read $ item env "Click"
                   }

item :: Env -> String -> String
-- default: leerer string
item env key = 
    case lookup key env of
        Just val  -> val
        Nothing   -> ""


