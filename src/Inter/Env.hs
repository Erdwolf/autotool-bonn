module Inter.Env where

--   $Id$

-- liest parameter-Werte aus CGI-Env

import qualified Inter.Param as P
import qualified Passwort

type Env = [(String, String)]

get :: Env  -> P.Type
-- default: leerer string
get env = get_with env P.example

get_with :: Env -> P.Type -> P.Type
get_with env def = 
    let input' = 
             if (item env "change") == "change" 
				|| (item env "Beispiel") == "Beispiel"
             then ""
             else item env "input"
		in
    if null $ item env "matrikel"
     then def
     else 
        let wahl = item env "wahl" in 
        -- auswahl in combobox
        if head ( item env "wahl")  == '-'
          then -- ja
          P.empty { P.matrikel = item env "matrikel" 
                  , P.passwort = read $ item env "passwort"
                  , P.problem  = item env "problem"
                  , P.aufgabe  = item env "aufgabe"
                  , P.version  = item env "version"
                  , P.input    = input' --item env "input"
                  }
          else -- nein, dann lies eingabefelder aus
          
          let 
          { pav = words $ map mySub wahl ;
            mySub ':' = ' ' ;
            mySub '-' = ' ' ;
            mySub x@_   = x ;
          }
          in      P.empty { P.matrikel = item env "matrikel" 
                              , P.passwort = read $ item env "passwort"
                              , P.problem  = pav !! 0
                              , P.aufgabe  = pav !! 1
                              , P.version  = pav !! 2
                              , P.input    = input' ---item env "input"
                              }

item :: Env -> String -> String
-- default: leerer string
item env key = 
    case lookup key env of
        Just val  -> val
        Nothing   -> ""


