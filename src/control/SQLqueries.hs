module SQLqueries 

( module SQLqueries
, ATHighLow (..)
, ATBewertung (..)
)

where

-- $Id$

import Database.MySQL.HSQL
import IO
import Char -- toLower

import Helper


-- TODO:
-- Schluessel zum Studenten nur noch SNr nicht mehr Mat

-- DB Helper
-- Header extrahieren aus SQL
showColTypes :: Statement -> [ String ]
showColTypes stat = [ s | (s,t,b) <- getFieldsTypes stat ]


-- --------------------------------------------------------------------------------
-- DB Funktionen
-- --------------------------------------------------------------------------------

-- liefert bewertete Aufgaben von mat aus DB,
-- TODO: mat = [] sollte auch StudentMNr als Spalte zurückliefern
studAufgDB :: String -> IO ( [ String ] , [ [ StrOrInt ] ])
studAufgDB mat =
	do
	   conn <- connect "localhost" "autoan" "test" "test"
	   stat <- query conn
			   ( concat
				 [ "SELECT "
				 , "vorlesung.Name As Vorlesung, "
				 , "aufgabe.Name AS Typ, "
				 , "aufgabe.Subject AS Nr, "
				 , "stud_aufg.Ok AS Ok, "
				 , "stud_aufg.No AS No "
				 , "FROM	student, aufgabe, stud_aufg , vorlesung "
				 , "WHERE	stud_aufg.SNr = student.SNr "
				 , "AND		stud_aufg.ANr = aufgabe.ANr "
				 , "AND		vorlesung.VNr = aufgabe.VNr "
				   -- wenn mat übergeben nur diese sonst alle
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


-- Neuen Student einfügen
insertNewStudentDB :: String -> String -> String -> String -> String -> IO ()
insertNewStudentDB vnm nme mat eml ps1 =
	do
	conn <- connect "localhost" "autoan" "test" "test"
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



-- Passt mnr <-> passwort , passwort = Nothing -> ohne Kontrolle
-- return Vornamen, Namen, Email, Status
checkPasswdMNrDB :: Maybe String -> String -> IO [ ( String , String , String , String ) ]
checkPasswdMNrDB maybePass mnr =
	do
	   conn <- connect "localhost" "autoan" "test" "test"
	   stat <- query conn
			   ( concat
				 [ "SELECT student.MNr AS MNr, \n"
				 , "student.Vorname AS Vorname, \n"
				 , "student.Name AS Name, \n"
				 , "student.Email AS Email, \n"
				 , "student.Status AS Status \n"
				 , "FROM	autoan.student \n"
				 , "WHERE	student.MNr = \"" ++ (filterQuots mnr)++ "\" "
				 , case maybePass of
				   Just pass	-> "AND student.Passwort = \"" ++ (quoteQuots pass)++ "\" "
				   Nothing		-> ""
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

-- liefert IO Maybe SNr zurück, wenn (mnr,pass) in DB
loginDB :: String -> String -> IO (Maybe String)
loginDB mnr pass =
	do
	   conn <- connect "localhost" "autoan" "test" "test"
	   stat <- query conn
			   ( concat
				 [ "SELECT "
				 , "SNr \n"
				 , "FROM	autoan.student \n"
				 , "WHERE	student.MNr = \"" ++ (filterQuots mnr)++ "\" "
				 , "AND student.Passwort = \"" ++ (quoteQuots pass)++ "\" "
				 , ";"
				 ] )
	   inh <- collectRows ( \ state -> do
							a <- getFieldValue state "SNr"
							return (a :: String)
						  ) stat
	   disconnect conn
	   return $ if null inh then Nothing else Just $ inh !! 0


-- Registrierung Vorabcheck auf Duplikate in Mnr, Email
-- return ( mnr Duplikat :: Bool , email Duplikat ::Bool)
duplMatOrEmailDB :: String -> String -> IO ( Bool , Bool )
duplMatOrEmailDB mat eml = 
	do 
	   conn <- connect "localhost" "autoan" "test" "test"
	   stat <- query conn 
			   ( concat 
				 [ "SELECT student.MNr, student.Email \n"
				 , "FROM	autoan.student \n"
				 , "WHERE	student.MNr = \""  
				 , filterQuots mat , "\" "
				 , "OR		student.Email = \""
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


-- Alle mgl. Vorlesungen 
getAllVorlesungenDB :: IO [ String ]
getAllVorlesungenDB = 
	do 
	   conn <- connect "localhost" "autoan" "test" "test"
	   stat <- query conn 
			   ( concat 
				 [ "SELECT vorlesung.Name AS Vorlesung \n"
				 , "FROM  autoan.vorlesung \n"
				 , ";"
				 ] )
	   inh <- collectRows ( \ state -> do
							a <- getFieldValue state "Vorlesung"
							return a
						  ) stat
	   disconnect conn 
	   return inh

-- Vorlesungen von Mnr
studVorlDB :: String -> IO [ String ]
studVorlDB mnr = 
	do 
	   conn <- connect "localhost" "autoan" "test" "test"
	   stat <- query conn 
			   ( concat 
				 [ "SELECT vorlesung.Name AS Vorlesung \n"
				 , "FROM	autoan.student , autoan.stud_vorl , autoan.vorlesung \n"
				 , "WHERE	student.MNr = \"" 
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

-- bepunktete Vorlesungen von Mnr 
getVorlesungWithPointsDB :: String -> IO [ String ]
getVorlesungWithPointsDB mnr = 
	do 
	   conn <- connect "localhost" "autoan" "test" "test"
	   stat <- query conn
			   ( concat 
				 [ "SELECT	vorlesung.Name AS Vorlesung \n"
				 , "FROM	autoan.vorlesung , autoan.stud_aufg , autoan.student , autoan.aufgabe\n"
				 , "WHERE	student.SNr = stud_aufg.SNr "
				 , "AND		student.MNr = \"" 
				 , filterQuots mnr , "\" "
				 , "AND		vorlesung.VNr = aufgabe.VNr "
				 , "AND		aufgabe.ANr = stud_aufg.ANr "  
				 , ";"
				 ] )
	   inh <- collectRows ( \ state -> do
							a <- getFieldValue state "Vorlesung"
							return a
						  ) stat
	   disconnect conn 
	   return inh

-- Email von Mnr ändern
-- TODO email validierung
updateEmailDB :: String -> String -> IO ()
updateEmailDB mat email =
	do
	   conn <- connect "localhost" "autoan" "test" "test"
	   stat <- query conn 
			   ( concat 
				 [ "UPDATE student "
				 , "SET Email= \"" 
				 , filterQuots email , "\" "
				 , "WHERE	student.MNr = \"" 
				 , filterQuots mat , "\" "
				 , ";"
				 ] )
	   disconnect conn 
	   return ()

-- Passwort - Änderung
updatePasswortDB :: String -> String -> IO ()
updatePasswortDB mat pass =
	do
	   conn <- connect "localhost" "autoan" "test" "test"
	   stat <- query conn 
			   ( concat 
				 [ "UPDATE student "
				 , "SET Passwort= \"" , quoteQuots pass , "\" "
				 , "WHERE	student.MNr = \"" , filterQuots mat , "\" "
				 , ";"
				 ] )
	   disconnect conn 
	   return ()

-- Vorlesung hinzufügen 
insertStudVorlDB :: String -> String -> IO ()
insertStudVorlDB mat vorl =
	do
	   conn <- connect "localhost" "autoan" "test" "test"
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
	   conn <- connect "localhost" "autoan" "test" "test"
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
-- 	[ ("gruppe.Name"			, "Gruppe")
	  -- 	, ("vorlesung.Name"		, "Vorlesung")
	  -- 	, ("gruppe.Referent"		, "Referent")
	  -- 	, ("gruppe.MaxStudents"	, "MaxStundenten")
	  -- 	, ("COUNT(stud_grp.SNR)"	, "AnzahlStudenten")
	  -- 	]
	  -- 
	  -- getGruppenDB_FROM = 
	  -- 	(  "autoan"
			   -- 	, ["gruppe","vorlesung","stud_grp"]
			   -- 	)
			   -- 
			   -- -- verknüpfen
				  -- getGruppenDB_WHERE_EQ =
				  -- 	[ ("gruppe.GNr","stud_grp.GNr")
						  -- 	, ("gruppe.VNr","vorlesung.VNr")
						  -- 	]
						  -- 
						  -- sqlselect s_as = "SELECT " ++ rowas' ++ " \n"
						  -- 	where 
						  -- 		rowas  = [  tbr ++ " AS " ++ tbn |  (tbr,tbn) <- s_as ]
						  -- 		rowas' = foldr1 (kommas) rowas
						  -- 
						  -- sqlwhere weqs = "WHERE " ++ wheq' ++ " \n"
						  -- 	where
						  -- 		wheq	= [ l ++ " = " ++ r | (l,r) <- weqs ]
						  -- 		wheq'	= foldr1 (andop) wheq
						  -- 		andop   = \a b -> a ++ " AND " ++b
						  --
						  -- sqlfrom fs = "FROM " ++ f' ++ " \n"
						  -- 	where
						  -- 		dbnme		= fst fs
						  -- 		f			= [ dbnme ++ "." ++ x | x <- snd fs ]
						  -- 		f'			= foldr1 (kommas) f


-- Übungsgruppen
-- liefert alle freien Gruppen
getFreeGruppenDB =
	do
	   conn <- connect "localhost" "autoan" "test" "test"
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
			-- verbinde gruppe mit stud_grp über GNr
			++ "autoan.gruppe LEFT JOIN autoan.stud_grp USING (GNr) ,"
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
	conn <- connect "localhost" "autoan" "test" "test"
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
	conn <- connect "localhost" "autoan" "test" "test"
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
   conn <- connect "localhost" "autoan" "test" "test"
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
		 ; conn <- connect "localhost" "autoan" "test" "test"
		 ; stat <- query conn $ "DELETE FROM autoan.stud_grp" ++ " " ++ "WHERE stud_grp.SNr = \"" ++ filterQuots (snrh!!0) ++ "\" " ++ ";"
	     ; stat <- query conn $ "INSERT INTO autoan.stud_grp (SNr,GNr) VALUES (" ++ filterQuots (snrh!!0) ++ "," 
							++ filterQuots (show grp) ++");"
		 ; disconnect conn 
		 ; return ()
		 }

-- ================================================================================
-- fürs AUTOTOOL

-- erhöht von Student, für Aufgabe (Ok,Size) / No 
bepunkteStudentDB :: String -> String -> ATBewertung -> ATHighLow -> IO ()
bepunkteStudentDB snr anr bewert highlow = do
   conn <- connect "localhost" "autoan" "test" "test"
   -- wenn (snr,anr) bereits in db -> update der Zeile sonst insert Zeile
   stat <- query conn ("SELECT SNr FROM stud_aufg \n" ++ 
					   "WHERE SNr = \"" ++ filterQuots snr ++ "\" "++
					   "AND ANr = \"" ++ filterQuots anr ++ "\" " ++
					   ";"
					  )
   inh <- collectRows ( \ state -> do
						b <- getFieldValue state "SNr"
						return (b :: String)
					  ) stat

   if null inh  
	  then  -- insert 
	    query conn 
			  ( concat 
				[ "INSERT INTO stud_aufg (SNr,ANr,Ok,No,Size,Scoretime) VALUES \n" 
				, "( \"" ++ filterQuots snr ++ "\" "
				, ", \"" ++ filterQuots anr ++ "\""
				, "," 
				, case bewert of 
				  No	-> "0,1,NULL,\"0000-00-00 00:00:00\"" 
				  Ok s	-> "1,0," ++ 
					( case highlow of 
					  Keine	-> "0,\"0000-00-00 00:00:00\""
					  _	-> show s ++ ", NOW()"
					)
				, " )"
				, ";"
				] 
			  )
	  else	-- update
		query conn
			 ( concat 
			   [ "UPDATE stud_aufg \n"
			   , "SET \n"
			   , case bewert of 
				 No	-> "No = No + 1 "
				 Ok s	-> "Ok = Ok + 1 " ++
					(	case highlow of 
						Keine	-> " " 
						High	-> " " 
							++ ", Scoretime = IF( Size > " ++ show s 
							++ " ,Scoretime, Now())"
							++ ", Size = GREATEST( Size," ++ show s ++ ")" 

						Low	-> " "
							++ ", Scoretime = IF( Size < " ++ show s
							++ " ,Scoretime, Now())"
							++ ", Size = LEAST( Size," ++ show s ++ ")" 

					)
			   , " \n"
			   , "WHERE SNr = \"" ++ filterQuots snr ++ "\" "
			   , "AND ANr = \"" ++ filterQuots anr ++ "\" "
			   , ";"
			 ] )
   disconnect conn 
   return ()



-- liefert (jetzt!)  mgl. Aufgaben für Student
-- [ ( ANr, Name , Subject , Path , Highscore ) ]

-- feature: wenn nr < 1000 (d. h. admin), dann nicht nach zeit fragen

mglAufgabenDB :: String -> IO [(String, String, String, String,String)]
mglAufgabenDB snr = do
        let timed = if False -- read snr < 1000 
		    then ""
		    else "AND NOW() BETWEEN Von AND Bis "
	conn <- connect "localhost" "autoan" "test" "test"
	stat <- query conn
			( concat
			  [ "SELECT aufgabe.ANr, aufgabe.Name , aufgabe.Subject , aufgabe.Path , aufgabe.Highscore \n"
			  , "FROM	aufgabe, gruppe , stud_grp \n"
			  , "WHERE \n"
			  , "gruppe.VNr = aufgabe.VNr \n"
			  , "AND gruppe.GNr = stud_grp.GNr \n"
			  , "AND stud_grp.SNr = \"" ++ filterQuots snr ++ "\" \n"
			  -- im Zeitfenster?
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

-- liefert (nun und demnaechst mgl). Aufgaben für Student/alle Student (snr=[])
-- ( [header ... ] , [ ( ANr, Name , Subject , Path , Highscore , Von , Bis ) ] )
mglNextAufgabenDB :: String -> IO ( [String],[[String]])
mglNextAufgabenDB snr = do
	conn <- connect "localhost" "autoan" "test" "test"
	stat <- query conn
			( concat
			  [ "SELECT aufgabe.ANr, aufgabe.Name AS Typ, aufgabe.Subject AS Nr, aufgabe.Highscore \n"
			  , ", DATE_FORMAT( aufgabe.Von , \"%H:%i %a %d. %b %Y\") as Von " 
			  , ", DATE_FORMAT( aufgabe.Bis , \"%H:%i %a %d. %b %Y\") as Bis\n"
			    --			  , ", aufgabe.Von " 
			    --			  , ", aufgabe.Bis "
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
checkAdminNamePasswortDB :: String -> String -> IO Bool
-- ADMIN
checkAdminNamePasswortDB nme pas = do
	   conn <- connect "localhost" "autoan" "test" "test"
	   stat <- query conn
			   ( concat
				 [ "SELECT \n"
				 , "admin.Name AS Name \n"
				 , "FROM	autoan.admin \n"
				 , "WHERE	\n"
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
--checkAdminNamePasswortDB :: String -> String -> IO Bool
failDB nme pas = do
	   conn <- connect "localhost" "autoan" "test" "test"
	   stat <- query conn 
			   ( concat 
				 [ "SELECT"  
				 , "admin.Name AS Name \n"
				 , "FROM	autoan.admin \n"
				 , "WHERE	\n"
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
findStudDB vnm nme mat eml vrl= do
	   conn <- connect "localhost" "autoan" "test" "test"
	   let 
			-- TODO optinale Teilwort suche (%part%)
			-- TODO or Verknüpfung
			ifexist x sqlnme =
				if length x > 0 
				then "AND "++ sqlnme ++ " LIKE \"%" ++ (quoteQuots x)++ "%\" "
				else ""
	   stat <- query conn 
			   ( concat 
				 [ "SELECT DISTINCT \n"
				 , "student.SNr		AS SNr , \n"
				 , "student.MNr		AS MNr, \n"
				 , "student.Vorname AS Vorname, \n"
				 , "student.Name	AS Name, \n"
				 , "student.Email	AS Email, \n"  
				 , "student.Status	AS Status \n"
				 , "FROM	autoan.student , autoan.vorlesung , autoan.stud_vorl \n"
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
updateStudDB oldmat mat vnm nme eml pas = do
   conn <- connect "localhost" "autoan" "test" "test"
   stat <- query conn
		   ( concat 
			 [ "UPDATE student \n"
			 , "SET \n"
			 , "student.MNr = \""		++ quoteQuots mat ++ "\", " 
			 , "student.Vorname = \""	++ quoteQuots vnm ++ "\", " 
			 , "student.Name = \""		++ quoteQuots nme ++ "\", "
			 , "student.Email = \""		++ quoteQuots eml ++ "\", "
			 , "student.Passwort = \""	++ quoteQuots pas ++ "\" "
			 , "WHERE student.MNr = \"" ++ quoteQuots oldmat ++ "\" "
			 ] )
   disconnect conn 
   return ()
--
getStudentDB mnr = do 
	conn <- connect "localhost" "autoan" "test" "test"
	stat <- query conn 
			( concat 
			  [ "SELECT student.MNr AS MNr, \n"
			  , "student.Vorname AS Vorname, \n"
			  , "student.Name AS Name, \n"
			  , "student.Email AS Email, \n"  
			  , "student.Status AS Status, \n"
			  , "student.Passwort AS Passwort \n"
			  , "FROM	autoan.student \n"
			  , "WHERE	student.MNr = \"" ++ (filterQuots mnr)++ "\" "
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
-- input: email-adresse
-- output: ( Id, Matrikel )
--
getIdMat :: String -> IO ( Maybe ( String, String ))
getIdMat email = do 
	conn <- connect "localhost" "autoan" "test" "test"
	putStrLn $ "connected"
	stat <- query conn $ unlines
	   [ "SELECT SNr, Email, MNr"
	   , "FROM	autoan.student"
	   , "WHERE	Email = \"" ++ (filterQuots email)++ "\""
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



