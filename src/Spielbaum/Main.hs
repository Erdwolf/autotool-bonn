module Main where

import Spielbaum.Spielbaum
import System

main :: IO ()
main = do
    argv <- System.getArgs
    if 3 > length argv 
      then info
      else if even ( length argv )
             then do ( file, _, exitCode) <- buildTree (tail argv) (tail $ head argv)
                     if exitCode == ExitSuccess
                       then do exitCode <- system ("rm " ++ (fst(span(/= '.')file))
                                                   ++".dot")
                               putStrLn ("Ausgabe ist in Datei: " ++ file)
                       else putStrLn ("Fehler bei der Benutzung von GraphViz")
             else do ( file, _, exitCode ) <- buildTree argv "" 
                     if exitCode == ExitSuccess
                       then do exitCode <- system ("rm " ++ (fst(span(/= '.')file))
                                                   ++".dot")
                               putStrLn ("Ausgabe ist in Datei: " ++ file)
                       else putStrLn ("Fehler bei der Benutzung von GraphViz")

info :: IO ()
info = putStrLn $ unlines
      [ "ERZEUGT SPIELBAUM (DAG) eines Wort-Ersetzungs-Spiels"
      , "===================================================="
      , "Mindestargumente: eine Regel und ein Startwort"
      , "    Argumente: l_1 r_1 l_2 r_2 .. l_n r_n Startwort"
      , "    Beispiel: 01 10 1 2 011"
      , "    interpretiert als Regelmenge { l_1 -> r_1, .. } und Startwort w"
      , "-------------------------------------------------------------------"
      , "Ausgabe: mit GraphViz gerendert in das File `Startwort+Option.ps'"
      , "-------------------------------------------------------------------"
      , "Optionen:"
      , "    Das Bild des Graph kann Kombinationen von Spielposition,"
      , "    GV-Wert und Grundy-Wert enthalten, einstellbar über:"
      , "          -> Spielposition, GV-Wert, Grundy-Wert"
      , "    -gv   -> Spielposition, GV-Wert"
      , "    -gr   -> Spielposition, Grundy-Wert"
      , "    -gg   -> GV-Wert, Grundy-Wert"
      , "    -l    -> Spielposition"
      , "    -vv   -> GV-Wert"
      , "    -rr   -> Grundy-Wert"
      , "    -no   -> leerer Baum"
      ]

