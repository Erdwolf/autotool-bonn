module Control.Queries where

--  $Id$

import Control.SQL
import Control.Types


import Inter.Crypt

import Control.Monad ( guard )
import Data.Maybe ( maybeToList )
import Data.List ( intersperse )
import Data.Typeable

#ifdef HSQL16
import Database.HSQL.MySQL ( SqlBind )
#elif HSQL14
import Database.HSQL.MySQL ( SqlBind )
#elif HSQL12
import Database.MySQL.HSQL ( SqlBind )
#endif

import Helper
import Mysqlconnect 

-- | Header extrahieren aus SQL
showColTypes :: Statement -> [ String ]
showColTypes stat = [ s | (s,t,b) <- getFieldsTypes stat ]


--  ------------------------------------------------------------------------------
-- DB Funktionen
--  ------------------------------------------------------------------------------

wrapped msg act = do
    logged $  msg ++ " ... "
    x <- act
    logged $ " ... " ++ msg
    return x

-- | liefert bewertete Aufgaben von mat aus DB,
-- TODO: mat = [] sollte auch StudentMNr als Spalte zurückliefern
studAufgDB :: Maybe MNr -> IO ( [ String ] , [ [ StrOrInt ] ])
studAufgDB mat = wrapped "studAufgDB" $ do
       conn <- myconnect 
       stat <- squery conn $
            Query ( Select (      [ reed "vorlesung.Name as Vorlesung" 
				  , reed "aufgabe.Name as Name"
				  , reed "aufgabe.Typ as Typ"
				  , reed "stud_aufg.Ok as Ok"
				  , reed "stud_aufg.No as No"
				  ] 
			   )
		  )
		  [ From  $ map reed [ "student", "aufgabe", "stud_aufg", "vorlesung" ] 
		  , Where $ ands
		          $ [ reed "stud_aufg.SNr = student.SNr"
			    , reed "stud_aufg.ANr = aufgabe.ANr"
			    , reed "vorlesung.VNr = aufgabe.VNr"
			    ] ++ 
		            [ equals (reed "student.MNr") (toEx mnr) 
			    | mnr <- maybeToList mat
			    ]
		  ]
       inh <- collectRows ( \ state -> do
                            v <- getFieldValue state "Vorlesung"
                            s <- getFieldValue state "Name"
                            t <- getFieldValue state "Typ"
                            Oks o <- getFieldValue state "Ok"
                            Nos n <- getFieldValue state "No"
                            return [ S v, S s , S t, I o , I n  ] -- FIXME
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
insertNewStudentDB vnm nme mat eml ps1 = wrapped "insertNewStudentDB" $ do
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
checkPasswdMNrDB maybePass mat = wrapped "checkPasswdMNrDB" $ do
       conn <- myconnect
       state <- squery conn $ Query 
               ( Select $      [ reed "student.MNr as MNr"
			       , reed "student.Vorname as Vorname"
			       , reed "student.Name as Name"
			       , reed "student.Email as Email"
			       , reed "student.Passwort as Passwort"
			       , reed "student.Status as Status"
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
duplMatOrEmailDB mat eml = wrapped "duplMatOrEmailDB" $ do 
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
getAllVorlesungenDB = wrapped "getAllVorlesungenDB" $ do 
       conn <- myconnect
       stat <- squery conn $ Query
	       ( Select $      [ reed "vorlesung.Name" ] )
               [ From [ reed "vorlesung" ]]
       inh <- collectRows ( \ state -> do
                            a <- getFieldValue state "Name"
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
getVorlesungWithPointsDB mnr = wrapped "getVorlesungWithPointsDB" $ do 
       conn <- myconnect
       stat <- squery conn $ Query
               ( Select $ [ reed "vorlesung.Name as Vorlesung" ] ) 
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
updateEmailDB mat email = wrapped "updateEmailDB" $ do
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
updatePasswortDB mat pass = wrapped "updatePasswortDB" $ do
       cpass <- Inter.Crypt.encrypt pass

       conn <- myconnect
       stat <- squery conn $ Query
	       ( Update ( reed "student" )
		        [ ( reed "Passwort", EString $ show cpass ) ] )
               [ Where $ equals (reed "student.MNr") (toEx mat) ] 
       disconnect conn 
       return ()


-- | Übungsgruppen
-- liefert alle freien Gruppen
getFreeGruppenDB :: IO ( [String], [ (GNr, [String]) ] )
getFreeGruppenDB = wrapped "getFreeGruppenDB" $ do
       conn <- myconnect
       stat <- query conn $
            "SELECT \n"
            ++ "gruppe.GNr as GNr, "
            ++ "vorlesung.Name as Vorlesungen,"
            ++ "gruppe.Name as Gruppen,"
            ++ "gruppe.Referent as Referent, "
            -- Maximum, Aktuelle Anzahl pro Gruppe
            ++ "gruppe.MaxStudents as studentMax, "
            ++ "COUNT(SNr) as studentCount" ++ " "
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

getAllGruppenDB :: IO ( [String], [ (GNr, [String]) ] )
getAllGruppenDB = wrapped "getAllGruppenDB" $ do
    conn <- myconnect
    state <- query conn $
            "SELECT \n"
            ++ "gruppe.GNr as GNr, "
            ++ "vorlesung.Name as Vorlesungen,"
            ++ "gruppe.Name as Gruppen,"
            ++ "gruppe.Referent as Referent "
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

getGruppenStudDB :: MNr -> IO [ GNr ]
getGruppenStudDB ( mat :: MNr ) = 
  wrapped ( "getAllGruppenDB " ++ show mat ) $   do
    conn <- myconnect
    stat <- squery conn $ Query 
            ( Select $      [ reed "stud_grp.GNr as GNr" ] )
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

getSNrFromMatDB :: MNr -> IO [ SNr ]
getSNrFromMatDB ( mat :: MNr ) = 
  wrapped ( "getSNrFromMatDB: start collecting for " ++ show mat ) $ do
    conn <- myconnect
    stat <- squery conn $ Query 
             ( Select $      [ reed "student.SNr as SNr" ] )
             [ From [ reed "student" ]
	     , Where $ equals ( reed "student.MNr" ) ( toEx mat )
	     ]
    inh  <- collectRows ( \ state -> do
                         snr <- getFieldValue state "SNr"
                         return ( snr :: SNr )
                       ) stat
    disconnect conn  
    return inh

-- | if student ist bereits in gruppe zu gleicher vorlesung,
-- then diese ändern, else gruppe hinzufügen

changeStudGrpDB  mat grp =  
    changeStudGrpDB' mat (fromCGI grp )

changeStudGrpDB' :: MNr -> GNr -> IO ()
changeStudGrpDB' mnr gnr = 
  wrapped (  "changeStudGrpDB" ++ show (mnr, gnr) ) $   do
    snrs <- getSNrFromMatDB mnr
    case snrs of
       _ | 1 /= length snrs -> return ()
       [ snr ] -> do
         conn <- myconnect
         stat <- squery conn $ Query
	     ( Delete $ reed "stud_grp"  )
	     [ Using $ map reed [ "stud_grp", "gruppe as g1", "gruppe as g2" ]
             , Where $ ands
	          [ equals ( reed "stud_grp.SNr" ) ( toEx snr )
                  , equals ( reed "stud_grp.GNr" ) ( reed "g1.GNr" )
		  , equals ( reed "g2.GNr" ) ( toEx gnr )
	          , equals ( reed "g1.VNr" ) ( reed "g2.VNr" )
		  ]
	     ]
         stat <- squery conn $ Query
	     ( Insert ( reed "stud_grp" )
                      [ ( reed "SNr", toEx snr )
		      , ( reed "GNr", toEx gnr )
		      ]
	     ) []
         disconnect conn 
         return ()

leaveStudGrpDB mat grp = leaveStudGrpDB' mat ( fromCGI grp )

leaveStudGrpDB' :: MNr -> GNr -> IO ()
leaveStudGrpDB' mnr gnr =  
  wrapped ( "leaveStudGrpDB" ++ show (mnr, gnr) ) $   do
    snrs <- getSNrFromMatDB mnr
    case snrs of
       _ | 1 /= length snrs -> return ()
       [ snr ] -> do
         conn <- myconnect
         stat <- squery conn $ Query
	     ( Delete $ reed "stud_grp"  )
	     [ Where $ ands
	          [ equals ( reed "stud_grp.SNr" ) ( toEx snr )
                  , equals ( reed "stud_grp.GNr" ) ( toEx gnr )
		  ]
	     ]
         disconnect conn 
         return ()

-- tricky
data Col a = Col String
data Cola = forall a . ( Show a, SqlBind a ) => Cola (Col a)

-- | liefert (jetzt!)  mgl. Aufgaben für Student
-- FIXME: use Control.Aufgabe type

mglAufgabenDB :: SNr -> IO [ (( ANr, Name, Typ),( Config, HiLo, Remark)) ]
mglAufgabenDB snr = mglAufgabenDB' False snr 

mglAufgabenDB' :: Bool 
    -> SNr
           -> IO [(( ANr, Name, Typ), ( Config, HiLo, Remark)) ]
mglAufgabenDB' isAdmin snr = 
  wrapped ( "mglAufgabenDB" ++ show (isAdmin, snr) ) $ do
    let cols =  [ "ANr", "Name", "Typ", "Config", "Highscore", "Remark" ]
    conn <- myconnect
    stat <- squery conn $ Query
	    ( Select $ do 
	         col <- cols
                 let long = Id [ "aufgabe", col ] ; short = Id [ col ]
                 return $ Bind ( EId long ) ( Just short )
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
                         return ( ( a , n, t) , (c, h, r) )
                       ) stat
    disconnect conn
    return inh





----------------------------------------------------------------------------------


-- | liefert (nun und demnaechst mgl). Aufgaben für Student

-- >  bzw. alle Student (snr=[])
-- > ( [header ... ] , [ ( ANr, Name , Subject , Path , Highscore , Von , Bis ) ] ) 
mglNextAufgabenDB_old :: SNr -> IO ( [String],[[String]])
mglNextAufgabenDB_old snr = 
  wrapped ( "mglNextAufgabenDB" ++ show snr ) $ do
    let ssnr = toString snr
    conn <- myconnect
    stat <- query conn
            ( concat
              [ "SELECT aufgabe.ANr, aufgabe.Typ as Typ, aufgabe.Name as Name, aufgabe.Highscore \n"
              , ", DATE_FORMAT( aufgabe.Von , \"%H:%i %a %d. %b %Y\") as Von " 
              , ", DATE_FORMAT( aufgabe.Bis , \"%H:%i %a %d. %b %Y\") as Bis\n"
              , "FROM aufgabe "
              , if null ssnr 
                then " \n" 
                else ", gruppe, stud_grp \n"
              , "WHERE \n"
              , if null ssnr 
                then 
                " "
                else
                "gruppe.VNr = aufgabe.VNr \n" ++
                "AND gruppe.GNr = stud_grp.GNr \n" ++
                "AND stud_grp.SNr = \"" ++ filterQuots ssnr ++ "\" \n" ++
                "AND \n" 
              -- noch offene Aufg.
              , "NOW() < Bis "
              , ";"
              ] )
    inh <- collectRows ( \ state -> do
                 a <- getFieldValue state "ANr"
                 b <- getFieldValue state "Name"
                 c <- getFieldValue state "Typ"
                 h <- getFieldValue state "Highscore"
                 vo <- getFieldValue state "Von"
                 bi <- getFieldValue state "Bis"
                 return [  a, b , c , h ,vo , bi ]
               ) stat
    disconnect conn
    return ( showColTypes stat, inh )



-- | liefert bewertete Aufgaben von mat aus DB,
-- TODO: mat = [] sollte auch StudentMNr als Spalte zurückliefern
mglNextAufgabenDB :: SNr -> IO ( [String],[[StrOrInt]])
mglNextAufgabenDB snr = wrapped "mglNextAufgabenDB" $ do
       conn <- myconnect 
       stat <- squery conn $
            Query ( Select (      [ reed "vorlesung.Name as Vorlesung" 
				  , reed "aufgabe.Name as Name"
				  , reed "aufgabe.Typ as Typ"
				  , reed "aufgabe.Von as Von"
				  , reed "aufgabe.Bis as Bis"
				  ] 
			   )
		  )
		  [ From  $ map reed [ "stud_grp", "aufgabe"
				     , "gruppe", "vorlesung" ] 
		  , Where $ ands
		          $ [ equals (toEx snr) (reed "stud_grp.SNr") 
			    , reed "stud_grp.GNr = gruppe.GNr"
			    , reed "gruppe.VNr = vorlesung.VNr"
			    , reed "vorlesung.VNr = aufgabe.VNr"
			    , reed "NOW() BETWEEN ( Von AND Bis )"
			    ] 
		  ]
       inh <- collectRows ( \ state -> do
                            v <- getFieldValue state "Vorlesung"
                            s <- getFieldValue state "Name"
                            t <- getFieldValue state "Typ"
                            ( von :: Time ) <- getFieldValue state "Von"
                            ( bis :: Time ) <- getFieldValue state "Bis"
                            return [ S v, S s , S t
				   , S $ toString von , S $ toString bis  
				   ]
                           ) stat
       disconnect conn
       return ( showColTypes stat, inh )




