module Turing_Test obsolete
 
( test
)

where

-- $Log$
-- Revision 1.2  2003-04-17 06:43:59  joe
-- Auswertng entfernt
--
-- Revision 1.1  2003/04/14 05:47:20  joe
-- drift/todoc/reader
--
-- Revision 1.1.1.1  2002/05/24 10:46:48  challenger
-- start
--
-- Revision 1.2  2002/04/08 11:32:25  autotool
-- turing updates
--

import Turing
import Turing_Konfiguration

import Turing_Akzeptieren
import Turing_Vorrechnen

import Auswertung
import Right
import Monad (guard)


test :: (TUM y z)
     => Int -> [[y]] -> [[y]]
     -> Turing y z 
     -> IO String

test cut pos neg m = do
    putStrLn $ "Ihre Turingmaschine ist"
    putStrLn $ show m

    putStrLn $ "Bei den folgenden Rechnungen berücksichtige ich"
    putStrLn $ "nur die ersten " ++ show cut ++ " erreichbaren Konfigurationen."

    muss (check m) $ do

      putStrLn $ "ich starte die Maschine auf einigen Wörtern aus L\n"
      let (offen, geheim) = splitAt 2 pos
      vorrechnens m $ offen
      putStrLn $ "\nweitere Eingaben: " ++ show geheim ++ "\n"

      muss (positiv_liste cut m pos) $ do

        putStrLn $ "ich starte die Maschine auf einigen Wörtern nicht in L\n"
        let (offen, geheim) = splitAt 2 neg
        vorrechnens m $ offen
        putStrLn $ "\nweitere Eingaben: " ++ show geheim ++ "\n"

	muss (negativ_liste cut m neg) $

	   right


