module SQLqueries 

( module SQLqueries
, ATHighLow (..)
, ATBewertung (..)
)

where

--   $Id$

import Database.MySQL.HSQL
import IO
import Char -- toLower

import Helper

import Mysqlconnect 


-- TODO:
-- Schluessel zum Studenten nur noch SNr nicht mehr Mat
-- Email-Validierung

-- ========================================
-- DB Helper

-- Header extrahieren aus SQL
showColTypes :: Statement -> [ String ]
showColTypes stat = [ s | (s,t,b) <- getFieldsTypes stat ]



--  ------------------------------------------------------------------------------
-- DB Funktionen
--  ------------------------------------------------------------------------------

-- liefert bewertete Aufgaben von mat aus DB,
-- TODO: mat = [] sollte auch StudentMNr als Spalte zur�ckliefern
studAufgDB :: String -> IO ( [ String ] , [ [ StrOrInt ] ])
studAufgDB mat =
    do
       conn <- myconnect 
       stat <- query conn
               ( concat
                 [ "SELECT "
                 , "vorlesung.Name As Vorlesung, "
                 , "aufgabe.Name AS Typ, "
                 , "aufgabe.Subject AS Nr, "
                 , "stud_aufg.Ok AS Ok, "
                 , "stud_aufg.No AS No "
                 , "FROM    student, aufgabe, stud_aufg , vorlesung "
                 , "WHERE   stud_aufg.SNr = student.SNr "
                 , "AND     stud_aufg.ANr = aufgabe.ANr "
                 , "AND     vorlesung.VNr = aufgabe.VNr "
                   -- wenn mat �bergeben nur diese sonst alle
                 , if (length mat) > 0
                   then "AND student.MNr = \"" ++ (filterQuots mat)  ++ "\" "
                   else ""
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            v <- getFieldValue state "Vorlesung"
                            t <- getFieldValue state "Typ"
                            s <- getFieldValue state "Nr"
                            o <- getFieldValue state "Ok"
                            n <- getFieldValue state "No"
                            return [ S v, S t, S s , I o , I n  ]
                          ) stat
       disconnect conn
       return ( showColTypes stat, inh )


-- Neuen Student einf�gen
-- TODO EMail-Ueberpruefung
insertNewStudentDB :: String -> String -> String -> String -> String -> IO ()
insertNewStudentDB vnm nme mat eml ps1 =
    do
    conn <- myconnect 
    stat <- query conn
            ( concat
              [ "INSERT INTO student \n"
              , "(Vorname,Name,MNr,Email,Passwort) \n"
              , "VALUES ( "
              , "\"" , filterQuots vnm , "\" , "
              , "\"" , filterQuots nme , "\" , "
              , "\"" , filterQuots mat , "\" , "
              , "\"" , filterQuots eml , "\" , "
              , "\"" , quoteQuots ps1 , "\" )"
              , ";"
              ] )
    disconnect conn
    return ()


--
-- Login des Studenten Version 1
--
-- Passt mnr <-> passwort 
-- Input:   Matrikelnr, passwort (= Nothing -> ohne Kontrolle)
-- Output:  IO [ (Vornamen, Namen, Email, Status) ] oder [] bei Fehler
checkPasswdMNrDB :: Maybe String -> String -> IO [ ( String , String , String , String ) ]
checkPasswdMNrDB maybePass mnr =
    do
       conn <- myconnect
       stat <- query conn
               ( concat
                 [ "SELECT student.MNr AS MNr, \n"
                 , "student.Vorname AS Vorname, \n"
                 , "student.Name AS Name, \n"
                 , "student.Email AS Email, \n"
                 , "student.Status AS Status \n"
                 , "FROM    student \n"
                 , "WHERE   student.MNr = \"" ++ (filterQuots mnr)++ "\" "
                 , case maybePass of
                   Just pass    -> "AND student.Passwort = \"" ++ (quoteQuots pass)++ "\" "
                   Nothing      -> ""
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            a <- getFieldValue state "Vorname"
                            b <- getFieldValue state "Name"
                            c <- getFieldValue state "Email"
                            d <- getFieldValue state "Status"
                            return (  a,  b , c , d )
                          ) stat
       disconnect conn
       return inh

--
-- Login des Studenten Version 2
--
-- Input:   Matrikelnr., Passwort
-- Output:  IO Just SNr zur�ck, wenn (mnr,pass) in DB
--
loginDB :: String -> String -> IO (Maybe String)
-- loginDB "" "" = return $ Nothing
loginDB mnr pass =
    do
       conn <- myconnect
       stat <- query conn
               ( concat
                 [ "SELECT "
                 , "SNr \n"
                 , "FROM    student \n"
                 , "WHERE   student.MNr = \"" ++ (filterQuots mnr)++ "\" "
                 , "AND student.Passwort = \"" ++ (quoteQuots pass)++ "\" "
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            a <- getFieldValue state "SNr"
                            return (a :: String)
                          ) stat
       disconnect conn
       return $ if null inh then Nothing else Just $ inh !! 0


-- 
-- Existiert mat oder email in DB?
-- 
-- Benutzt bei Registrierung als Vorabcheck auf Duplikate in Mnr, Email
-- Input:   Matrikelnr., Email-Adresse
-- Ouput:   ( mnr Duplikat :: Bool , email Duplikat ::Bool)
--
duplMatOrEmailDB :: String -> String -> IO ( Bool , Bool )
duplMatOrEmailDB mat eml = 
    do 
       conn <- myconnect
       stat <- query conn 
               ( concat 
                 [ "SELECT student.MNr, student.Email \n"
                 , "FROM    student \n"
                 , "WHERE   student.MNr = \""  
                 , filterQuots mat , "\" "
                 , "OR      student.Email = \""
                 , filterQuots eml , "\" "
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            n <- getFieldValue state "MNr"
                            e <- getFieldValue state "Email"
                            return ( n , e )
                          ) stat
       let mORe  = ( ( length [ m | m <- (Prelude.map fst inh), m == mat ] ) > 0
                   , ( length [ e | e <- (Prelude.map snd inh), e == eml ] ) > 0
                   )
       disconnect conn 
       return mORe


--
-- Liefert alle mgl. Vorlesungen 
--
-- Input:
-- Output:  IO [ Vorlesungsname ]
--
getAllVorlesungenDB :: IO [ String ]
getAllVorlesungenDB = 
    do 
       conn <- myconnect
       stat <- query conn 
               ( concat 
                 [ "SELECT vorlesung.Name AS Vorlesung \n"
                 , "FROM  vorlesung \n"
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            a <- getFieldValue state "Vorlesung"
                            return a
                          ) stat
       disconnect conn 
       return inh

-- Vnr -> [ Mnr ]
teilnehmer :: String -> IO [ String ]
teilnehmer vnr = do
       conn <- myconnect
       state <- query conn 
               ( unwords
                 [ "SELECT student.mnr"
                 , "FROM   student, stud_grp, gruppe"
                 , unwords [ "WHERE gruppe.vnr = ", filterQuots vnr ]
                 , "AND stud_grp.gnr = gruppe.gnr"
                 , "AND student.snr = stud_grp.snr"
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            a <- getFieldValue state "mnr"
                            return a
                          ) state
       disconnect conn 
       return inh

--
-- Liefer Vorlesungen von Matrikelnr.
--
-- Input:   Matrikelnr
-- Output:  IO [ Vorlesungsname ]
--
studVorlDB :: String -> IO [ String ]
studVorlDB mnr = 
    do 
       conn <- myconnect
       stat <- query conn 
               ( concat 
                 [ "SELECT vorlesung.Name AS Vorlesung \n"
                 , "FROM    student , stud_vorl , vorlesung \n"
                 , "WHERE   student.MNr = \"" 
                 , filterQuots mnr , "\" "
                 , "AND stud_vorl.SNr = student.SNr \n"
                 , "AND stud_vorl.VNr = vorlesung.VNr "
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            a <- getFieldValue state "Vorlesung"
                            return a
                          ) stat
       disconnect conn 
       return inh

--
-- Liefert bepunktete Vorlesungen von Matrikelnr.
--
-- Input:   Matrikelnr.
-- Output:  IO [ Vorlesungsname ]
--
getVorlesungWithPointsDB :: String -> IO [ String ]
getVorlesungWithPointsDB mnr = 
    do 
       conn <- myconnect
       stat <- query conn
               ( concat 
                 [ "SELECT  vorlesung.Name AS Vorlesung \n"
                 , "FROM    vorlesung , stud_aufg , student , aufgabe\n"
                 , "WHERE   student.SNr = stud_aufg.SNr "
                 , "AND     student.MNr = \"" 
                 , filterQuots mnr , "\" "
                 , "AND     vorlesung.VNr = aufgabe.VNr "
                 , "AND     aufgabe.ANr = stud_aufg.ANr "  
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            a <- getFieldValue state "Vorlesung"
                            return a
                          ) stat
       disconnect conn 
       return inh

--
-- Modifiziert Email-Adresse von Matrikelnr.
--
-- Input:   Matrikelnr., Email-Adresse
-- Output:  IO ()
--
-- TODO email validierung
--
updateEmailDB :: String -> String -> IO ()
updateEmailDB mat email =
    do
       conn <- myconnect
       stat <- query conn 
               ( concat 
                 [ "UPDATE student "
                 , "SET Email= \"" 
                 , filterQuots email , "\" "
                 , "WHERE   student.MNr = \"" 
                 , filterQuots mat , "\" "
                 , ";"
                 ] )
       disconnect conn 
       return ()

--
-- Modifiziert Passwort von Matrikelnr.
--
-- Input:   Matrikelnr., Passwort
-- Output:  IO () 
--
updatePasswortDB :: String -> String -> IO ()
updatePasswortDB mat pass =
    do
       conn <- myconnect
       stat <- query conn 
               ( concat 
                 [ "UPDATE student "
                 , "SET Passwort= \"" , quoteQuots pass , "\" "
                 , "WHERE   student.MNr = \"" , filterQuots mat , "\" "
                 , ";"
                 ] )
       disconnect conn 
       return ()

-- Vorlesung hinzuf�gen 
insertStudVorlDB :: String -> String -> IO ()
insertStudVorlDB mat vorl =
    do
       conn <- myconnect
       stat1 <- query conn
                ( concat
                  [ "SELECT student.SNr, vorlesung.VNr "
                  , "FROM student, vorlesung "
                  , "WHERE student.MNr = \"" , filterQuots mat , "\" "
                  , "AND vorlesung.Name =\"" , filterQuots vorl , "\" "
                  , ";"
                  ]
                )
       inh <- collectRows ( \ state -> do
                            s <- getFieldValue state "SNr"
                            v <- getFieldValue state "VNr"
                            return ( s  , v )
                          ) stat1
       let { snr = fst (inh!!0) ; vnr = snd (inh!!0) } 
       stat <- query conn 
               ( concat 
                 [ "INSERT INTO stud_vorl (SNr,VNr) VALUES "
                 , "( " ++ snr ++ "," ++ vnr ++ ")"
                 , ";"
                 ] )
       disconnect conn 
       return ()

-- Vorlesung entfernen
removeStudVorlDB :: String -> String -> IO ()
removeStudVorlDB mat vorl =
    do
       conn <- myconnect
       stat1 <- query conn
                ( concat
                  [ "SELECT student.SNr, vorlesung.VNr "
                  , "FROM student, vorlesung "
                  , "WHERE student.MNr = \"" , filterQuots mat , "\" "
                  , "AND vorlesung.Name =\"" , filterQuots vorl , "\" "
                  , ";"
                  ]
                )
       inh <- collectRows ( \ state -> do
                            s <- getFieldValue state "SNr"
                            v <- getFieldValue state "VNr"
                            return ( s  , v )
                          ) stat1
       let { snr = fst (inh!!0) ; vnr = snd (inh!!0) } 
       stat <- query conn 
               ( concat 
                 [ "DELETE FROM stud_vorl "
                 , "WHERE stud_vorl.SNr = " , filterQuots snr , " " 
                 , "AND stud_vorl.VNr = " , filterQuots vnr , " "
                 , ";"
                 ] )
       disconnect conn 
       return ()

-- TEST: Select 
-- getGruppenDB_SELECT_AS =
--  [ ("gruppe.Name"            , "Gruppe")
      --    , ("vorlesung.Name"     , "Vorlesung")
      --    , ("gruppe.Referent"        , "Referent")
      --    , ("gruppe.MaxStudents" , "MaxStundenten")
      --    , ("COUNT(stud_grp.SNR)"    , "AnzahlStudenten")
      --    ]
      -- 
      -- getGruppenDB_FROM = 
      --    (  "
               --   , ["gruppe","vorlesung","stud_grp"]
               --   )
               -- 
               --   verkn�pfen
                  -- getGruppenDB_WHERE_EQ =
                  --    [ ("gruppe.GNr","stud_grp.GNr")
                          --    , ("gruppe.VNr","vorlesung.VNr")
                          --    ]
                          -- 
                          -- sqlselect s_as = "SELECT " ++ rowas' ++ " \n"
                          --    where 
                          --        rowas  = [  tbr ++ " AS " ++ tbn |  (tbr,tbn) <- s_as ]
                          --        rowas' = foldr1 (kommas) rowas
                          -- 
                          -- sqlwhere weqs = "WHERE " ++ wheq' ++ " \n"
                          --    where
                          --        wheq    = [ l ++ " = " ++ r | (l,r) <- weqs ]
                          --        wheq'   = foldr1 (andop) wheq
                          --        andop   = \a b -> a ++ " AND " ++b
                          --
                          -- sqlfrom fs = "FROM " ++ f' ++ " \n"
                          --    where
                          --        dbnme       = fst fs
                          --        f           = [ dbnme ++ "." ++ x | x <- snd fs ]
                          --        f'          = foldr1 (kommas) f


-- �bungsgruppen
-- liefert alle freien Gruppen
getFreeGruppenDB =
    do
       conn <- myconnect
       stat <- query conn $
            "SELECT \n"
            ++ "gruppe.GNr AS GNr, "
            ++ "vorlesung.Name AS Vorlesungen,"
            ++ "gruppe.Name AS Gruppen,"
            ++ "gruppe.Referent AS Referent, "
            -- Maximum, Aktuelle Anzahl pro Gruppe
            ++ "gruppe.MaxStudents AS studentMax, "
            ++ "COUNT(SNr) AS studentCount" ++ " "
            ++ "\nFROM \n"
            -- verbinde gruppe mit stud_grp �ber GNr
            ++ "gruppe LEFT JOIN stud_grp USING (GNr) ,"
            ++ "vorlesung" ++ " "
            ++ "\nWHERE \n" ++" gruppe.VNr = vorlesung.VNr "   ++" "
            ++ "\nGROUP BY \n" ++ "GNr "
            -- nur nicht volle gruppen.
            ++ "\nHAVING \n"   ++ "studentCount" ++ " < " ++ " studentMax " ++ " "
            ++ "\nORDER BY \n" ++ "Vorlesungen, Gruppen " ++ ";"
       inh <- collectRows ( \ state -> do
                            k <- getFieldValue state "GNr"
                            v <- getFieldValue state "Vorlesungen"
                            g <- getFieldValue state "Gruppen"
                            r <- getFieldValue state "Referent"
                            m <- getFieldValue state "studentMax"
                            c <- getFieldValue state "studentCount"
                            return ( k :: Int ,
                                     [ v :: String
                                     , g :: String
                                     , r :: String
                                     , m :: String
                                     , c :: String ]
                                   )
                          ) stat
       disconnect conn
       return ( showColTypes stat, inh )

getAllGruppenDB = do
    conn <- myconnect
    stat <- query conn $
            "SELECT \n"
            ++ "gruppe.GNr AS GNr, "
            ++ "vorlesung.Name AS Vorlesungen,"
            ++ "gruppe.Name AS Gruppen,"
            ++ "gruppe.Referent AS Referent "
            ++ "\nFROM vorlesung , gruppe "
            ++ "\nWHERE vorlesung.VNr = gruppe.VNr "
            ++ "\nORDER BY Vorlesungen,Gruppen,Referent " ++ "\n"
    inh <- collectRows ( \ state -> do
                            k <- getFieldValue state "GNr"
                            v <- getFieldValue state "Vorlesungen"
                            g <- getFieldValue state "Gruppen"
                            r <- getFieldValue state "Referent"
                            return ( k :: Int ,
                                     [ v :: String
                                     , g :: String
                                     , r :: String
                                     ]
                                   )
                          ) stat
    disconnect conn
    return ( showColTypes stat, inh )

getGruppenStudDB mat =
    do
    conn <- myconnect
    stat <- query conn
            ( "SELECT stud_grp.GNr as GNr" ++ " \n"
           ++ "FROM stud_grp, student" ++ " \n"
           ++ "WHERE stud_grp.SNr = student.SNr" ++ " \n"
           ++ "AND student.MNr = " ++"\"" ++ filterQuots mat ++ "\""
           ++ ";" )
    inh  <- collectRows ( \ state -> do
                          g <- getFieldValue state "GNr"
                          return (g :: Int)
                        ) stat
    disconnect conn 
    return inh

getSNrFromMatDB mat = do
   conn <- myconnect
   stat <- query conn $ "SELECT student.SNr AS SNr FROM student WHERE student.MNr =" ++ "\"" ++ filterQuots mat ++ "\""
   inh  <- collectRows ( \ state -> do
                         snr <- getFieldValue state "SNr"
                         return ( snr :: String)
                       ) stat
   disconnect conn  
   return inh

changeStudGrpDB mat grp = 
    do
    snrh <- getSNrFromMatDB mat
    if null snrh 
      then return ()
      else  
      do {
         ; conn <- myconnect
         ; stat <- query conn $ "DELETE FROM stud_grp" ++ " " ++ "WHERE stud_grp.SNr = \"" ++ filterQuots (snrh!!0) ++ "\" " ++ ";"
         ; stat <- query conn $ "INSERT INTO stud_grp (SNr,GNr) VALUES (" ++ filterQuots (snrh!!0) ++ "," 
                            ++ filterQuots (show grp) ++");"
         ; disconnect conn 
         ; return ()
         }

-- ================================================================================
-- f�rs AUTOTOOL

--
-- erh�ht von Student, f�r Aufgabe (Ok,Size) / No 
--
-- Input: (SNr,ANr,{No,Ok size}, {High,Low,Keine} )
-- Output: IO ()
--
-- bepunkteStudentDB :: String -> String -> ATBewertung -> ATHighLow -> IO ()
-- bepunkteStudentDB snr anr bewert highlow = return ()

bepunkteStudentDB snr anr bewert highlow = do
   conn <- myconnect
   stat <- query conn ("SELECT SNr FROM stud_aufg \n" ++ 
                        "WHERE SNr = \"" ++ filterQuots snr ++ "\" "++
                        "AND ANr = \"" ++ filterQuots anr ++ "\" " ++
                        ";"
                       )
   inh <- collectRows ( \ state -> do
                        b <- getFieldValue state "SNr"
                        return (b :: String)
                      ) stat

   --
   -- wenn (snr,anr) bereits in db -> update der Zeile sonst insert Zeile
   --
   let tim s = case highlow of 
			   Keine -> "0,\"0000-00-00 00:00:00\""
			   _     -> show s ++ ", NOW()" 
   let insertsql = 
		   concat [ "INSERT INTO stud_aufg (SNr,ANr,Ok,No,Size,Scoretime) VALUES \n" 
				  , "( \"" ++ filterQuots snr ++ "\" "
				  , ", \"" ++ filterQuots anr ++ "\""
				  , "," 
					-- 
				  , case bewert of 
					No    -> "0,1,NULL,\"0000-00-00 00:00:00\"" 
					Ok s  -> "1,0," ++ tim s
				  , " )"
				  , ";"
				  ] 
   if null inh  
      then  -- insert 

      query conn insertsql

                
      else  -- update
      query conn
                ( concat 
                  [ "UPDATE stud_aufg \n"
                  , "SET \n"
                  , case bewert of 
                    No  -> "No = No + 1 "
                    Ok s   -> "Ok = Ok + 1 " ++ sizetime s
                  , " \n"
                  , "WHERE SNr = \"" ++ filterQuots snr ++ "\" "
                  , "AND ANr = \"" ++ filterQuots anr ++ "\" "
                  , ";"
                  ] )
   disconnect conn 
   return ()
     where sizetime s =
             case highlow of 
             Keine   -> " " 
             High    -> " "
                        ++ ", Scoretime = IF( IFNULL(Size, "++ show s ++" - 1) < " ++ show s 
                       ++ " , Now(), Scoretime )"
                       ++ ", Size = GREATEST( Size," ++ show s ++ ")" 
             Low -> " "
                    ++ ", Scoretime = IF( IFNULL(Size, "++ show s ++" + 1 ) > " ++ show s
                   ++ " ,Now(), Scoretime )"
                   ++ ", Size = LEAST( Size," ++ show s ++ ")" 




-- liefert (jetzt!)  mgl. Aufgaben f�r Student
-- [ ( ANr, Name , Subject , Path , Highscore ) ]

-- feature: wenn nr < 1000 (d. h. admin), dann nicht nach zeit fragen

mglAufgabenDB :: String -> IO [(String, String, String, String,String)]
mglAufgabenDB snr = mglAufgabenDB' False snr 

mglAufgabenDB' :: Bool -> String 
           -> IO [(String, String, String, String,String)]
mglAufgabenDB' isAdmin snr = 
	do
    let timed = if isAdmin 
				then ""
				else "AND NOW() BETWEEN Von AND Bis "
    conn <- myconnect
    stat <- query conn
            ( concat
              [ "SELECT aufgabe.ANr, aufgabe.Name , aufgabe.Subject , aufgabe.Path , aufgabe.Highscore \n"
              , "FROM   aufgabe, gruppe , stud_grp \n"
              , "WHERE \n"
              , "gruppe.VNr = aufgabe.VNr \n"
              , "AND gruppe.GNr = stud_grp.GNr \n"
              , "AND stud_grp.SNr = \"" ++ filterQuots snr ++ "\" \n"
              -- mit Zeitfenster?
              , timed
              , ";"
              ] )
    inh <- collectRows ( \ state -> do
                         a <- getFieldValue state "ANr"
                         b <- getFieldValue state "Name"
                         c <- getFieldValue state "Subject"
                         d <- getFieldValue state "Path"
                         e <- getFieldValue state "Highscore"
                         return (  a ,  b , c , d , e )
                       ) stat
    disconnect conn
    return inh

-- liefert (nun und demnaechst mgl). Aufgaben f�r Student/alle Student (snr=[])
-- ( [header ... ] , [ ( ANr, Name , Subject , Path , Highscore , Von , Bis ) ] )
mglNextAufgabenDB :: String -> IO ( [String],[[String]])
mglNextAufgabenDB snr = do
    conn <- myconnect
    stat <- query conn
            ( concat
              [ "SELECT aufgabe.ANr, aufgabe.Name AS Typ, aufgabe.Subject AS Nr, aufgabe.Highscore \n"
              , ", DATE_FORMAT( aufgabe.Von , \"%H:%i %a %d. %b %Y\") as Von " 
              , ", DATE_FORMAT( aufgabe.Bis , \"%H:%i %a %d. %b %Y\") as Bis\n"
                --            , ", aufgabe.Von " 
                --            , ", aufgabe.Bis "
              , "FROM aufgabe "
              , if null snr 
                then " \n" 
                else ", gruppe, stud_grp \n"
              , "WHERE \n"
              , if null snr 
                then 
                " "
                else
                "gruppe.VNr = aufgabe.VNr \n" ++
                "AND gruppe.GNr = stud_grp.GNr \n" ++
                "AND stud_grp.SNr = \"" ++ filterQuots snr ++ "\" \n" ++
                "AND \n" 
              -- noch offene Aufg.
              , "NOW() < Bis "
              , ";"
              ] )
    inh <- collectRows ( \ state -> do
                 a <- getFieldValue state "ANr"
                 b <- getFieldValue state "Typ"
                 c <- getFieldValue state "Nr"
                 h <- getFieldValue state "Highscore"
                 vo <- getFieldValue state "Von"
                 bi <- getFieldValue state "Bis"
                 return [  a, b , c , h ,vo , bi ]
               ) stat
    disconnect conn
    return ( showColTypes stat, inh )


-- ================================================================================
-- ADMIN


-- 
-- Login des Admins
-- 
-- Input:   Name, Passwort
-- Output:  IO True/False
--
checkAdminNamePasswortDB :: String -> String -> IO Bool
checkAdminNamePasswortDB nme pas = do
       conn <- myconnect
       stat <- query conn
               ( concat
                 [ "SELECT \n"
                 , "admin.Name AS Name \n"
                 , "FROM    admin \n"
                 , "WHERE   \n"
                 , "admin.Name = \"" ++ (quoteQuots nme)++ "\" "
                 , "AND admin.Passwort = \"" ++ (quoteQuots pas)++ "\" "
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                    b <- getFieldValue state "Name"
                    return (b :: String)
            ) stat
       disconnect conn
       let loginok = length inh > 0
       return loginok


failDB nme pas = do
       conn <- myconnect
       stat <- query conn 
               ( concat 
                 [ "SELECT"  
                 , "admin.Name AS Name \n"
                 , "FROM    admin \n"
                 , "WHERE   \n"
                 , "admin.Name = \"" ++ (quoteQuots nme)++ "\" "
                 , "AND admin.Passwort = \"" ++ (quoteQuots pas)++ "\" "
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            b <- getFieldValue state "Name"
                            return (b :: String)
                          ) stat
       disconnect conn
       let loginok = length inh > 0
       return loginok

--
-- Liefert Studenten-Daten die auf Vorname,Name,Matrikel,Email und Vorlesung passen.
--
-- Hinweis: Und-Verknuepfung, wobei leere (="") Parameter ignoriert werden.
-- Input: Vorname,Name,Matrikel,Email,Vorlesungsname
-- Output: IO [ [ SNr, MNr, Vorname , Email, Status ] ]
--
-- TODO optinale Teilwort suche (%part%)
-- TODO optinale or Verkn�pfung?
--
findStudDB vnm nme mat eml vrl= do
       conn <- myconnect
       let 
            ifexist x sqlnme =
                if length x > 0 
                then "AND "++ sqlnme ++ " LIKE \"%" ++ (quoteQuots x)++ "%\" "
                else ""
       stat <- query conn 
               ( concat 
                 [ "SELECT DISTINCT \n"
                 , "student.SNr     AS SNr , \n"
                 , "student.MNr     AS MNr, \n"
                 , "student.Vorname AS Vorname, \n"
                 , "student.Name    AS Name, \n"
                 , "student.Email   AS Email, \n"  
                 , "student.Status  AS Status \n"
                 , "FROM    student , vorlesung , stud_vorl \n"
                 , "WHERE "
                 , "student.SNr = stud_vorl.SNr " 
                 , "AND stud_vorl.VNr = vorlesung.VNr " 
                 , ifexist mat "student.MNr" 
                 , ifexist vnm "student.Vorname"
                 , ifexist nme "student.Name"
                 , ifexist eml "student.Email"
                 , ifexist vrl "vorlesung.Name"  
                 , "ORDER BY MNr "  
                 , ";"
                 ] )
       inh <- collectRows ( \ state -> do
                            s <- getFieldValue state "SNr"
                            m <- getFieldValue state "MNr"
                            a <- getFieldValue state "Vorname"
                            b <- getFieldValue state "Name"
                            c <- getFieldValue state "Email"
                            d <- getFieldValue state "Status"
                            return [ I s, S m , S a,  S b , S c , S d ]
                          ) stat
       disconnect conn 
       return (  showColTypes stat, inh )

--
-- Modifiziert vom Student Matrikel,Vorname,Name,Email,Passwort
--
-- Input:   AlteMatrikelnr., NeueMatrikelnr., Vorname,Name,Email,Passwort
-- Output:  IO ()
--
updateStudDB oldmat mat vnm nme eml pas = do
   conn <- myconnect
   stat <- query conn
           ( concat 
             [ "UPDATE student \n"
             , "SET \n"
             , "student.MNr = \""       ++ quoteQuots mat ++ "\", " 
             , "student.Vorname = \""   ++ quoteQuots vnm ++ "\", " 
             , "student.Name = \""      ++ quoteQuots nme ++ "\", "
             , "student.Email = \""     ++ quoteQuots eml ++ "\", "
             , "student.Passwort = \""  ++ quoteQuots pas ++ "\" "
             , "WHERE student.MNr = \"" ++ quoteQuots oldmat ++ "\" "
             ] )
   disconnect conn 
   return ()

--
-- Liefert vom Studenten: Vorname,Name,Email,Status,Passwort
--
-- Input: Matrikelnr.
-- Output: IO [( Vorname, Name , Email, Status, Passwort )]
--
getStudentDB mnr = do 
    conn <- myconnect
    stat <- query conn 
            ( concat 
              [ "SELECT student.MNr AS MNr, \n"
              , "student.Vorname AS Vorname, \n"
              , "student.Name AS Name, \n"
              , "student.Email AS Email, \n"  
              , "student.Status AS Status, \n"
              , "student.Passwort AS Passwort \n"
              , "FROM   student \n"
              , "WHERE  student.MNr = \"" ++ (filterQuots mnr)++ "\" "
              , ";"
              ] )
    inh <- collectRows ( \ state -> do
                         a <- getFieldValue state "Vorname"
                         b <- getFieldValue state "Name"
                         c <- getFieldValue state "Email"
                         d <- getFieldValue state "Status"
                         e <- getFieldValue state "Passwort"
                         return (  S a,  S b , S c , S d , S e )
                       ) stat
    disconnect conn 
    return inh

--
-- Liefert Email und Passwort von Matrikelnr
--
-- Input:   Matrikelnr
-- Output:  (Email,Passwort) 
--
getEmailPasswortDB :: String -> IO [(String, String)]
getEmailPasswortDB mnr = do 
    conn <- myconnect
    stat <- query conn 
            ( concat 
              [ "SELECT "
              , "student.Email AS Email, \n"  
              , "student.Passwort AS Passwort \n"
              , "FROM   student \n"
              , "WHERE  student.MNr = \"" ++ (filterQuots mnr)++ "\" "
              , ";"
              ] )
    inh <- collectRows ( \ state -> do
                    e <- getFieldValue state "Email"
                    p <- getFieldValue state "Passwort"
                    return (e,p)
               ) stat
    disconnect conn 
    return inh


--
-- input: email-adresse
-- output: ( Id, Matrikel )
--
getIdMat :: String -> IO ( Maybe ( String, String ))
getIdMat email = do 
    conn <- myconnect
    putStrLn $ "connected"
    stat <- query conn $ unlines
       [ "SELECT SNr, Email, MNr"
       , "FROM  student"
       , "WHERE Email = \"" ++ (filterQuots email)++ "\""
       -- kein semikolon? sonst absturz?
       ] 
    putStrLn $ "queried"
    inh <- collectRows ( \ state -> do
         a <- getFieldValue state "SNr"
         b <- getFieldValue state "MNr"
         return (  a, b )
                       ) stat
    putStrLn $ "collected"
    disconnect conn 
    putStrLn $ "disconnected"
    case inh of
         [ line ] -> return $ Just line
         _        -> return Nothing




-- ================================================================================
-- Auswertung
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
getSerienPunkteDB :: String -> IO [(String,String)]
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


-- liefert Serien Punkte von allen Studenten
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



-- Erweiterungen f�r den ScorerDB
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

---------------------------------------------------------------------------

{-
TODO: 
sowas geh�rt in ein extra modul
und sollte f�r jede SQL-tabelle existieren
(d. h. alle komponenten enthalten, und instance SQLBind ?)
-}

data Aufgabe  =
     Aufgabe { aufgabe :: String
	       , direction :: ATHighLow
	       , vorlesung :: String
	       }
     deriving ( Eq, Ord, Show, Read )

getHighscoreAufgabeTypesDB :: IO [ Aufgabe ]
getHighscoreAufgabeTypesDB =
    do
    { let { sqlstr = unwords
              [ "SELECT "
              , " CONCAT(aufgabe.Name,\"-\",aufgabe.Subject ) as aufg"
              , ", Highscore "
	      , ", VNr"
              , "FROM   aufgabe "
              , "ORDER BY aufg"
              , ";" 
              ]  
        }
    ; conn <- myconnect
    ; stat <- query conn $ sqlstr
    ;  inh  <- collectRows (\ state ->
                         do
                                a <- getFieldValue state "aufg"
                                b <- getFieldValue state "Highscore"
				c <- getFieldValue state "VNr"

                                return $ Aufgabe { aufgabe = a
						   , direction = read b
						   , vorlesung = c
						   }
                        ) stat
    ; return inh
    }


