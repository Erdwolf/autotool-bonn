module Control.Queries where

--  $Id$

import Control.SQL
import Control.Types


import Inter.Crypt

import Control.Monad ( guard )
import Data.Maybe ( maybeToList )
import Data.List ( intersperse )
import Data.Typeable

import Helper
import Mysqlconnect 

-- | Header extrahieren aus SQL
showColTypes :: Statement -> [ String ]
showColTypes stat = [ s | (s,t,b) <- getFieldsTypes stat ]


--  ------------------------------------------------------------------------------
-- DB Funktionen
--  ------------------------------------------------------------------------------

-- | liefert bewertete Aufgaben von mat aus DB,
-- TODO: mat = [] sollte auch StudentMNr als Spalte zurückliefern
studAufgDB :: Maybe MNr -> IO ( [ String ] , [ [ StrOrInt ] ])
studAufgDB mat =
    do
       conn <- myconnect 
       stat <- squery conn $
            Query ( Select (      [ reed "vorlesung.Name AS vorlesung" 
				  , reed "aufgabe.Name AS Typ"
				  , reed "aufgabe.Subject AS Nr"
				  , reed "stud_aufg.Ok AS Ok"
				  , reed "stud_aufg.No AS No"
				  ] 
			   )
		  )
		  [ From  $ map reed [ "student", "aufgabe", "stud_aufg", "vorlesung" ] 
		  , Where $ ands
		          $ [ reed "stud_aufg.SNr = student.SNr"
			    , reed "stud_aufg.ANr = aufgabe.ANr"
			    , reed "vorlesung.VNr = aufgabe.VNr"
			    ] ++ 
		            [ EBinop "=" (reed "student.MNr") (toEx mnr) 
			    | mnr <- maybeToList mat
			    ]
		  ]
       inh <- collectRows ( \ state -> do
                            v <- getFieldValue state "Vorlesung"
                            t <- getFieldValue state "Typ"
                            s <- getFieldValue state "Nr"
                            o <- getFieldValue state "Ok"
                            n <- getFieldValue state "No"
                            return [ S v, S t, S s , I o , I n  ] -- FIXME
                          ) stat
       disconnect conn
       return ( showColTypes stat, inh )




-- | Neuen Studenten einfügen
-- TODO: EMail-Ueberpruefung
insertNewStudentDB :: String -- ^ vorname
		   -> String -- ^ nachname
		   -> MNr -- ^ matrikel
		   -> String -- ^ email
		   -> String -- ^ passwort
		   -> IO ()
insertNewStudentDB vnm nme mat eml ps1 =    do
    cps1 <- encrypt ps1
    conn <- myconnect 
    stat <- squery conn $ Query
            ( Insert (reed "student")
                     [ (reed "Vorname", EString vnm)
		     , (reed "Name", EString nme)
		     , (reed "MNr", toEx mat)
		     , (reed "Email", EString eml)
		     , (reed "Passwort", EString $ show cps1)
		     ]
	      ) []
    disconnect conn
    return ()


-- |
-- Login des Studenten Version 1
--
-- Passt mnr <-> passwort 
-- Input:   Matrikelnr, passwort (= Nothing -> ohne Kontrolle)
-- Output:  IO [ (Vornamen, Namen, Email, Status) ] oder [] bei Fehler
checkPasswdMNrDB :: Maybe String 
		 -> MNr 
		 -> IO [ ( String , String , String , String ) ]
checkPasswdMNrDB maybePass mat =
    do
       conn <- myconnect
       state <- squery conn $ Query 
               ( Select $      [ reed "student.MNr AS MNr"
			       , reed "student.Vorname AS Vorname"
			       , reed "student.Name AS Name"
			       , reed "student.Email AS Email"
			       , reed "student.Passwort AS Passwort"
			       , reed "student.Status AS Status"
			       ] 
	       )
	       [ From [ reed "student" ]
	       , Where $ equals (reed "student.MNr") (toEx mat)
               ] 
       inh <- collectRows ( \ state -> do
                            a <- getFieldValue state "Vorname"
                            b <- getFieldValue state "Name"
                            c <- getFieldValue state "Email"
                            d <- getFieldValue state "Status"
                            e <- getFieldValue state "Passwort"
                            return (  a,  b , c , d , reed e )
                ) state
       disconnect conn
       
       return $ do
           (a, b, c, d, e) <- inh
           guard $ case maybePass of
                 Nothing   -> True
		 Just pass -> Inter.Crypt.compare e pass 
	   return ( a, b, c, d )

-- | 
-- Existiert mat oder email in DB?
-- 
-- Benutzt bei Registrierung als Vorabcheck auf Duplikate in Mnr, Email
-- Input:   Matrikelnr., Email-Adresse
-- Ouput:   ( mnr Duplikat :: Bool , email Duplikat ::Bool)
--
duplMatOrEmailDB :: MNr -> String -> IO ( Bool , Bool )
duplMatOrEmailDB mat eml = 
    do 
       conn <- myconnect
       stat <- query conn 
               ( concat 
                 [ "SELECT student.MNr, student.Email \n"
                 , "FROM    student \n"
                 , "WHERE   student.MNr = \""  
                 , show mat , "\" "
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


-- |
-- Liefert alle mgl. Vorlesungen 
--
-- Input:
-- Output:  IO [ Vorlesungsname ]
--
getAllVorlesungenDB :: IO [ String ]
getAllVorlesungenDB = 
    do 
       logged "getAllVorlesungenDB"
       conn <- myconnect
       stat <- squery conn $ Query
	       ( Select $      [ reed "vorlesung.Name AS Vorlesung" ] )
               [ From [ reed "vorlesung" ]]
       inh <- collectRows ( \ state -> do
                            a <- getFieldValue state "Vorlesung"
                            return a
                          ) stat
       disconnect conn 
       return inh


-- |
-- Liefert bepunktete Vorlesungen von Matrikelnr.
--
-- Input:   Matrikelnr.
-- Output:  IO [ Vorlesungsname ]
--
getVorlesungWithPointsDB :: MNr -> IO [ String ]
getVorlesungWithPointsDB mnr = 
    do 
       conn <- myconnect
       stat <- squery conn $ Query
               ( Select $      [ reed "vorlesung.Name AS Vorlesung" ] ) 
	       [ From $ map reed [ "vorlesung", "stud_aufg" , "student" , "aufgabe" ]
	       , Where $ ands [ reed "student.SNr = stud_aufg.SNr"
			      , equals (reed "student.MNr") (toEx mnr)
			      , reed "vorlesung.VNr = aufgabe.VNr"
			      , reed "aufgabe.ANr = stud_aufg.ANr"
			      ]
	       ]
       inh <- collectRows ( \ state -> do
                            a <- getFieldValue state "Vorlesung"
                            return a
                          ) stat
       disconnect conn 
       return inh

-- |
-- Modifiziert Email-Adresse von Matrikelnr.
--
-- Input:   Matrikelnr., Email-Adresse
-- Output:  IO ()
--
-- TODO email validierung
--
updateEmailDB :: MNr -> String -> IO ()
updateEmailDB mat email =
    do
       conn <- myconnect
       stat <- squery conn $ Query
	       ( Update ( reed "student" )
		        [ ( reed "Email", EString email ) ] )
               [ Where $ equals (reed "student.MNr") (toEx mat) ] 
       disconnect conn 
       return ()

-- |
-- Modifiziert Passwort von Matrikelnr.
--
-- Input:   Matrikelnr., Passwort
-- Output:  IO () 
--
updatePasswortDB :: MNr -> String -> IO ()
updatePasswortDB mat pass =
    do
       cpass <- Inter.Crypt.encrypt pass

       conn <- myconnect
       stat <- squery conn $ Query
	       ( Update ( reed "student" )
		        [ ( reed "Passwort", EString $ show cpass ) ] )
               [ Where $ EBinop "=" (reed "student.MNr") (toEx mat) ] 
       disconnect conn 
       return ()


-- | Übungsgruppen
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
            ++ "\nFROM gruppe \n"

            -- verbinde gruppe mit stud_grp über GNr
            ++ " LEFT JOIN stud_grp USING (GNr) "

            ++ ", vorlesung" ++ " "
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
                            -- m <- getFieldValue state "studentMax"
                            -- c <- getFieldValue state "studentCount"
                            return ( k :: GNr ,
                                     [ v :: String
                                     , g :: String
                                     , r :: String
                                     -- , m :: String
                                     -- , c :: String 
				     ]
                                   )
                          ) stat
       disconnect conn
       return ( showColTypes stat, inh )

getAllGruppenDB = do
    conn <- myconnect
    state <- query conn $
            "SELECT \n"
            ++ "gruppe.GNr AS GNr, "
            ++ "vorlesung.Name AS Vorlesungen,"
            ++ "gruppe.Name AS Gruppen,"
            ++ "gruppe.Referent AS Referent "
            ++ "\nFROM vorlesung , gruppe "
            ++ "\nWHERE vorlesung.VNr = gruppe.VNr "
            ++ "\nORDER BY Vorlesungen,Gruppen,Referent "
	    ++ " ;"
    logged "getAllGruppenDB: start collecting"
    inh <- collectRows ( \ s -> do
                            k <- getFieldValue s "GNr"
                            v <- getFieldValue s "Vorlesungen"
                            g <- getFieldValue s "Gruppen"
                            r <- getFieldValue s "Referent"
                            return ( k :: GNr
                                   , [  v :: String
                                     ,  g :: String
                                     ,  r :: String
                                     ]
                                   )
                          ) state
    logged "getAllGruppenDB: end collecting"
    disconnect conn
    return ( showColTypes state, inh )

getGruppenStudDB ( mat :: MNr ) =
    do
    conn <- myconnect
    stat <- squery conn $ Query 
            ( Select $      [ reed "stud_grp.GNr AS GNr" ] )
            [ From $ map reed [ "stud_grp", "student" ]
            , Where $ ands
	            [ equals (reed "stud_grp.SNr") (reed "student.SNr")
		    , equals (reed "student.MNr") (toEx mat)
		    ]
	    ]
    inh  <- collectRows ( \ state -> do
                          ( g :: GNr ) <- getFieldValue state "GNr"
                          return ( g )
                        ) stat
    disconnect conn 
    return inh

getSNrFromMatDB ( mat :: MNr ) = do
   conn <- myconnect
   stat <- squery conn $ Query 
             ( Select $      [ reed "student.SNr AS SNr" ] )
             [ From [ reed "student" ]
	     , Where $ equals ( reed "student.MNr" ) ( toEx mat )
	     ]
   inh  <- collectRows ( \ state -> do
                         snr <- getFieldValue state "SNr"
                         return ( snr :: String)
                       ) stat
   disconnect conn  
   return inh

-- | if student ist bereits in gruppe zu gleicher vorlesung,
-- then diese ändern, else gruppe hinzufügen
changeStudGrpDB mat grp =     do
    snrh <- getSNrFromMatDB mat
    if null snrh 
      then return ()
      else  
      do {
         ; conn <- myconnect
         ; stat <- query conn $ "DELETE FROM stud_grp USING stud_grp, gruppe AS g1, gruppe AS g2 "
	       ++ "WHERE stud_grp.SNr = \"" ++ filterQuots (snrh!!0) ++ "\" " 
               ++ "AND stud_grp.GNr = g1.GNr "
	       ++ "AND g2.GNR = " ++ filterQuots (show grp) ++ " "
	       ++ "AND g1.VNr = g2.VNr "
	       ++ ";"
         ; stat <- query conn $ "INSERT INTO stud_grp (SNr,GNr) VALUES (" ++ filterQuots (snrh!!0) ++ "," 
                            ++ filterQuots (show grp) ++");"
         ; disconnect conn 
         ; return ()
         }


leaveStudGrpDB mat grp =     do
    snrh <- getSNrFromMatDB mat
    if null snrh 
      then return ()
      else  
      do {
         ; conn <- myconnect
         ; stat <- query conn $ "DELETE FROM stud_grp  "
	       ++ "WHERE stud_grp.SNr = \"" ++ filterQuots (snrh!!0) ++ "\" " 
               ++ "AND stud_grp.GNr = " ++ filterQuots (show grp) ++ " "
	       ++ ";"
         ; disconnect conn 
         ; return ()
         }



-- | liefert (jetzt!)  mgl. Aufgaben für Student
-- FIXME: use Control.Aufgabe type

mglAufgabenDB :: SNr -> IO [ (( ANr, Name, Typ),( Config, HiLo, Remark)) ]
mglAufgabenDB snr = mglAufgabenDB' False snr 

mglAufgabenDB' :: Bool 
    -> SNr
           -> IO [(( ANr, Name, Typ), ( Config, HiLo, Remark)) ]
mglAufgabenDB' isAdmin snr = 	do
    conn <- myconnect
    stat <- squery conn $ Query
	    ( Select $ map reed 
	             [ "aufgabe.ANr", "aufgabe.Name", "aufgabe.Typ"
		     , "aufgabe.Config", "aufgabe.Highscore", "aufgabe.Remark" 
		     ]
	    )
            [ From $ map reed [ "aufgabe", "gruppe" , "stud_grp" ]
	    , Where $ ands $
	            [ equals ( reed "gruppe.VNr" ) ( reed "aufgabe.VNr" )
		    , equals ( reed "gruppe.GNr" ) ( reed "stud_grp.GNr" )
		    , equals ( reed "stud_grp.SNr" ) ( toEx snr )
		    ] ++ 
		    [ reed "NOW() BETWEEN ( Von AND Bis )"
		    | not isAdmin
		    ]
	    ]
    inh <- collectRows ( \ state -> do
                         a <- getFieldValue state "ANr"
                         n <- getFieldValue state "Name"
                         t <- getFieldValue state "Typ"
                         c <- getFieldValue state "Config"
                         h <- getFieldValue state "Highscore"
			 r <- getFieldValue state "Remark"
                         return ( ( a , n, t), (c, h, r) )
                       ) stat
    disconnect conn
    return inh


----------------------------------------------------------------------------------


{-

-- | liefert (nun und demnaechst mgl). Aufgaben für Student

-- >  bzw. alle Student (snr=[])
-- > ( [header ... ] , [ ( ANr, Name , Subject , Path , Highscore , Von , Bis ) ] ) 
mglNextAufgabenDB :: String -> IO ( [String],[[String]])
mglNextAufgabenDB snr = do
    conn <- myconnect
    stat <- query conn
            ( concat
              [ "SELECT aufgabe.ANr, aufgabe.Name AS Typ, aufgabe.Subject AS Nr, aufgabe.Highscore \n"
              , ", DATE_FORMAT( aufgabe.Von , \"%H:%i %a %d. %b %Y\") as Von " 
              , ", DATE_FORMAT( aufgabe.Bis , \"%H:%i %a %d. %b %Y\") as Bis\n"
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

-}