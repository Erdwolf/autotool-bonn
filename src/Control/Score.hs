module Control.Score where

-- | Auswertung
-- z.T. halbautomatisch!

--
-- in der Punkte Tabelle stehen alle Punkte
-- nach Serien zusammengefasst und getrennt 
-- schriftlichen / autotool
--

-- liefert Serien Punkte des Studenten
-- input:   Matrikel
-- output:  IO [(Serie,Punkte)]
--
getSerienPunkteDB :: MNr -> IO [(String,String)]
getSerienPunkteDB mnr =
    if null mnr then return []
    else do
    conn <- myconnect
    stat <- query conn $ sqlstr
    inh  <- collectRows (\ state ->
                         do
                         m <- getFieldValue state "Serie"
                         p <- getFieldValue state "Punkte"
                         return ( m :: String , p :: String )
                        ) stat
    return inh

    where sqlstr =
              concat [ "SELECT \n" 
             , "punkte AS Punkte , \n"
                     , "rubrik AS Serie \n"
                     , "FROM punkte \n"
                     , "WHERE \n" 
             , "Mnr = " , filterQuots mnr , " \n"
                     , "ORDER BY Serie;"
                     ]


-- | liefert Serien Punkte von allen Studenten
--
getAllSerienPunkteDB :: IO [(String,String,String,String)]
getAllSerienPunkteDB =
    do
    conn <- myconnect
    stat <- query conn $ sqlstr
    inh  <- collectRows 
			    (\ state ->
                 do     
                 n <- getFieldValue state "Name" ;
                 v <- getFieldValue state "Vorname" ;
                 p <- getFieldValue state "Punkte" ;
                 s <- getFieldValue state "Serie"
                 return ( n :: String , v :: String, s :: String , p :: String )
                ) stat
    return inh

    where sqlstr =
              concat [ "SELECT \n" 
             , "s.Name AS Name, \n"
             , "s.Vorname AS Vorname, \n"  
             , "p.punkte AS Punkte , \n"
                     , "p.rubrik AS Serie \n"
                     , "FROM punkte AS p , student AS s \n"
                     , "WHERE \n" 
             , "s.MNr = p.MNr \n" 
                     , "ORDER BY s.Name, s.Vorname, Serie ;"
                     ]



-- | Erweiterungen für den ScorerDB
getHighscoreCandidatesDB =
    do
    { let { sqlstr = foldr1 (\x y -> x ++ " " ++ y) 
              [ "SELECT student.MNr "
              , ", CONCAT(aufgabe.Name, aufgabe.Subject ) as aufg"
              , ", size "
              , ", Scoretime "  
              , "FROM   stud_aufg, aufgabe, student "
              , "WHERE  stud_aufg.ANr = aufgabe.ANr "
              , "AND    student.SNr = stud_aufg.SNr "
              , "AND    size >= 0 "
              , "AND    MNr > 1024 "
              , "ORDER BY aufg, size, Scoretime "
              , ";" 
              ]  
            ; sqlstr' = "SELECT * from student;"
        }
    ; conn <- myconnect
    ; stat <- query conn $ sqlstr
    ;  inh  <- collectRows (\ state ->
                         do
                                a <- getFieldValue state "aufg"
                                b <- getFieldValue state "MNr"
                                c <- getFieldValue state "size"
                                d <- getFieldValue state "Scoretime"

                                return ( a :: String , b :: String , c :: String , d :: String )
                        ) stat
    ; return inh
    }



