module Main where

import IO
import HTMLMonad 
import CGI
import Char -- toLower

-- autoan-modules
import HTMLshortcuts
import SQLqueries
import Helper

-- --
-- TODO 
-- SQL-Exception fangen
-- Seiten Struktur rausziehen: stdpage ttl bdy menu
main :: IO ()
main = 
	do
	run [] $ loginPage "" F0 --mainCGI 


-- Msg. die mehrmals auftauchen
msgKontrollEmail = do
	ttxt $ "Eine Kontroll-Email wird Ihn zugesandt, " ++ 
			 "diese muss innerhalb einer Woche beantwortet werden. " ++
			 "In Ihrer Antwort muss die zugesandte Kontrollzahl " ++ 
			 "im Subject (Betreff) stehen (z.B. \"Re: 1233456\"). " 
	smallspacerow
	ttxt $ "Erst danach k�nnen Sie mit dem Autotool arbeiten. " ++
			 "Den Anmeldestatus k�nnen Sie auf Ihrer Autotool-�bersicht Seite sehen."

msgPasswort = do 
	empty
--	ttxt $ "Als Passwort sind nur Worte aus deutschen Buchstaben [a-ZA-Z�������] und Zahlen [0-9] erlaubt."

-- Einstieg: loginPage 
-- folge Seiten
-- a) registerPage
-- b) checkLoginPage 
loginPage mat F0  = 
	standardQuery "Willkommen." $ do 
		table $ do 
			attr "width" "600"
			hrrow
			th3 "Hier k�nnen Sie Sich in die �bungsgruppen von Logik oder Berechenbarkeit/Komplexit�t einschreiben."
			spacerow
			ttxt "Dazu m�ssen Sie sich Neuanmelden und dann eine Gruppe w�hlen."
			spacerow
			spacerow
			ttxt $ concat 
					 [ "Wenn Sie bereits angemeldet sind, Matrikelnr. und Passwort eingeben." 
--					 , "ansonsten k�nnen Sie sich neuanmelden." 
					 ]
			ttxt $ "Dann k�nnen Sie Ihre Daten �ndern/ansehen."
			spacerow
			matF <- promptedInput		"Matrikelnr:"	$ (fieldSIZE 30) ## ( fieldVALUE mat )
			pwdF <- promptedPassword	"Passwort:"		(fieldSIZE 30)
			hrrow
			smallSubButton (F2 matF pwdF)	checkLoginPage	"Login" 
			smallSubButton F0				(registerPage "" "" "" "") "Neuanmeldung"


-- Neuanmeldung 								  
registerPage vnm nme mat eml F0 =
	standardQuery "Willkommen zur Neuammeldung" $ do
		table $ do	
			attr "width" "600"	
			hrrow
			th3 "Nach der Anmeldung k�nnen Sie die Vorlesung/�bungsgruppe ausw�hlen."
			spacerow
			ttxt "Bitte achten Sie auf die korrekte Angabe von Matrikelnummer und Email-Adresse."
			spacerow
			ttxt "Nur von dieser Adresse kommende Emails werden vom Autotool akzeptiert."
			ttxt $ "Notieren Sie bitte Ihr Passwort." 
			spacerow
			vnmF <- promptedInput	"Vorname"		$ (fieldSIZE 30) ## (fieldVALUE vnm )
			nmeF <- promptedInput	"Name"			$ (fieldSIZE 30) ## (fieldVALUE nme )
			matF <- promptedInput	"Matrikelnr."	$ (fieldSIZE 30) ## (fieldVALUE mat )
			emlF <- promptedInput	"Email-Adresse" $ (fieldSIZE 30) ## (fieldVALUE eml )
			spacerow
			msgPasswort
			ps1F <- promptedPassword "Passwort"		(fieldSIZE 30) 
			ps2F <- promptedPassword "Passwort Wdh." (fieldSIZE 30)
			hrrow
			smallSubButton (F6 vnmF nmeF matF emlF ps1F ps2F) registeredPage "Anmelden"
			smallSubButton F0 endPage "Ende"

-- �berpr�fung der angegeben Daten
registeredPage (F6 vnmF nmeF matF emlF ps1F ps2F) = 
	let 
		vnm = unNonEmpty		( value vnmF )
		nme = unNonEmpty		( value nmeF )
		mat = unAllDigits		( value matF )
		eml = unEmailAddress	( value emlF )
		ps1 = unNonEmpty		( value ps1F )
		ps2 = unNonEmpty		( value ps2F )
		matok = if allDigits mat then (read mat) > 1024 else False
	in
	do 
	-- suche nach dupletten von email or mat
	( matDup , emlDup ) <- io $ duplMatOrEmailDB mat eml
	let { checks = [ ( ps1 == ps2			, "Passwort und Passwort Wiederholung nicht identisch.")
 -- Q: kann mat hier nicht direkt checken?					 
				   , ( matok				, "Die Matrikelnummer muss gr��er 1024 sein.")
				   , ( length vnm > 1		, "Der Vorname muss mindestens 2 Buchstaben lang sein.") 
				   , ( length nme > 1		, "Der Name muss mindestens 2 Buchstaben lang sein.") 
				   , ( not matDup			, "Matrikelnummer bereits registiert.")
				   , ( not emlDup			, "Email bereits registiert.")
				   ]
		; allok = and [ fst c | c <- checks ]
		}

	if allok then io $ insertNewStudentDB vnm nme mat eml ps1 else return ()
	standardQuery "Neuanmeldung" $ do
		table $ do
			attr "width" "600"	
			hrrow
			if allok 
				then do
					 th3 "Vielen Dank. Alle Angaben wurden �bernommen. "
					 msgKontrollEmail
					 hrrow
					 smallSubButton F0 (changeGrpPage mat) "Weiter"
				else do
					 ttxt "Bitte �berpr�fen Sie Ihre Angaben:"
					 spacerow
					 mapM_ (ttxt) [  msg | ( ok , msg ) <- checks , not ok ]
					 hrrow
					 smallSubButton F0 (registerPage vnm nme mat eml) "Zur�ck"

-- EndPage
endPage F0 = 
	standardQuery "Ende" $ 
		table $ do 	
			hrrow
			th3 "Vielen Dank f�r die Benutzung. Und sch�nen Tag noch."
			hrrow


-- �berp�fung der passwort,matrikel paares, bei Erfolg Student-Status anzeigne
checkLoginPage (F2 matF pwdF) =
  let 
		mat  = unNonEmpty ( value matF )
		pass = unNonEmpty ( value pwdF )
  in
	do inh <- io $ checkPasswdMNrDB (Just pass) mat
	   if  ( (length inh)  == 0  ) -- passwort nicht okay?
		 then 
			do standardQuery "Fehler:" $ 
				table $ do
					hrrow		
					th3 "Passwort und Matrikelnummer stimmen nicht �berein!" 
					hrrow
					smallSubButton F0 (loginPage mat) "Login wiederholen"
					smallSubButton F0 (registerPage "" "" "" "")   "Neuanmeldung"
					smallSubButton F0 endPage		 "Ende"

		 else 
			do studStatusPage mat F0

studStatusPage mat F0 = 
	do 
	  inh		<- io $ checkPasswdMNrDB Nothing mat
	  result	<- io $ studAufgDB mat
	  (h,grps)	<- io $ getAllGruppenDB 
	  stdgrp    <- io $ getGruppenStudDB mat
	  let	grp = [ v ++ ", " ++  g ++ ", " ++ r |  (gnr , [v,g,r]) <- grps , gnr `elem` stdgrp ]
--	  vorl		<- io $ studVorlDB mat
	  standardQuery "�bersicht" $ 
		let ( vorname , name , email , status ) = ( inh !! 0 ) in	
		do 
			hrline
			h3 $ text "Anmeldungsdaten"
			table $ 
				do 
 				attr "cellspacing" "2" 
				attr "cellpadding" "5"
				tableRow2 (text "Name") ( text ( vorname ++ ", " ++ name ++ " " ) )
				tableRow2 (text "Matrikelnr.") (text mat)
				tableRow2 (text "Email-Adresse") (text email)
				tableRow2 (text "Anmeldestatus") (text status)
 				tableRow2 (text "Vorlesung/Gruppe") $ text $ if null grp then "keine" else show grp
			if ( length (snd result)) > 0 
			   then do { h3 $ text "Ergebnisse" ; ( showAsTable result ) }
			   else do { h3 $ text "keine Ergebnisse" } 

			-- --------------------------------------------------
			hrline
			h3 $ text "Hier k�nnen Sie :"
			table $ 
				do 
				   smallSubButton F0 (changeGrpPage mat)			"Vorlesung/�bungsgruppe ausw�hlen/�ndern"
				   smallspacerow
				   smallSubButton F0 (changeEmailPage mat email)	"die Email-Adresse �ndern"
				   smallSubButton F0 (changePasswortPage mat)		"das Passwort �ndern"
				   smallspacerow
				   smallSubButton F0 (studStatusPage mat)			"�bersicht aktualisieren"
				   smallspacerow
				   smallSubButton F0 endPage						"beenden"

-- Vorlesung hinzuf�gen
addVorlesungPage mat vorl F0 = do
	vls <- io getAllVorlesungenDB
	standardQuery "Vorlesung hinzuf�gen" $ 
		let 
			mglVl = [ v | v <- vls , not ( v `elem` vorl ) ]
		in	
		do
		table $ do 
			attr "width" "600"
			hrrow
			if length mglVl > 0 
			 then
			    do 
				tr $ td $ text "Bitte w�hlen Sie eine Vorlesung aus:"
				addVlF <- tr $ td $ selectSingle show Nothing mglVl empty
				hrrow
				smallSubButton (F1 addVlF) (addedVorlesungPage mat) "Hinzuf�gen"
				return ()
			 else
			   do
			   tr $ td $ text "Sie haben bereits alle Vorlesungen ausgew�hlt."
			   hrrow
			smallSubButton F0 (studStatusPage mat) "Zur�ck"

addedVorlesungPage mat (F1 addVlF) = do 
	let addVl = value $ addVlF 
	io $ insertStudVorlDB mat addVl
	(h, grps)<- io $ getFreeGruppenDB
--TODO addvl argument in getGruppenStudDB
	studgrps <- io $ getGruppenStudDB mat 
	let mglgrp = if null studgrps then []
				 else
				 [ ( gnr , g ++ " " ++ r ) 
				 | ( gnr , [v,g,r,c,m]) <- grps 
				 ]
		
	standardQuery "Vorlesung hinzuf�gt, �bungsgruppe ausw�hlen" $ 
		do	
		table $ do
			attr "width" "600"
			hrrow
			ttxt $ "Vorlesung " ++ addVl ++ " hinzugef�gt."
			if null mglgrp 
			   then -- keine mglgruppen?
			   do 
			   if null grps -- warum: keine freie grp oder student ist schon in �bgrp
					then
					ttxt "Alle �bungsgruppen sind belegt. Wenden Sie sich an die Gruppenleiter."
					else 
					ttxt "Sie besuchen bereits ein �bungsgruppe der Vorlesung."
			   hrrow
			   smallSubButton F0 (studStatusPage mat) "Weiter"
			   else -- auswahl der mgl. �bungsgrpen
				do
				th3  "Jetzt m�ssen Sie eine �bungsgruppe ausw�hlen. "
				ttxt "Es werden nur �bungsgruppen die noch nicht voll sind angezeigt."
				let show' = show . snd
				addgrpF <- tr $ td $ selectSingle show' Nothing mglgrp empty
				hrrow
--				smallSubButton (F1 addgrpF) (addedGrpPage mat) "Weiter"
			--			

-- TODO gruppen wechseln nach Bepunktung verhindern
changeGrpPage mat F0 = do
	  (h,freegrps)	<- io $ getFreeGruppenDB 
	  (h,allgrps)   <- io $ getAllGruppenDB
	  stdgrp		<- io $ getGruppenStudDB mat
	  let	
		posgrp       = [ (gnr ,v ++ ", " ++  g ++ ", " ++ r) |  (gnr , [v,g,r,c,m]) <- freegrps , not $ gnr `elem` stdgrp ]
		currentgrps	 = [ (gnr ,v ++ ", " ++  g ++ ", " ++ r) |  (gnr , [v,g,r]) <- allgrps , gnr `elem` stdgrp ] 	  
		thrd (_,_,x) = x
	  standardQuery "�bungsgruppe �ndern" $ 
			table $ 
				  do
				  th3  "Bitte  w�hlen Sie Ihre �bungsgruppe. "
				  ttxt $ "Aktuelle �bungsgruppe: " ++ if null currentgrps then "keine" else show $ Prelude.map snd currentgrps
				  let { show' = show . snd }
				  if null posgrp 
					 then ttxt "Leider sind alle Gruppen voll."	>> hrrow >> smallSubButton F0 (studStatusPage mat) "Weiter"
					 else 
					 do
					 ttxt "Es werden nur �bungsgruppen die noch nicht voll sind angezeigt."
					 grpF <- tr $ td $ selectSingle show' Nothing posgrp empty
					 hrrow
					 smallSubButton (F1 grpF) (changedGrpPage mat) "Weiter"			
					 smallSubButton F0 (studStatusPage mat) "Zur�ck"

changedGrpPage mat (F1 grpF) = do 
	let (grp,desc) =value $ grpF
	io $ changeStudGrpDB mat grp   
	standardQuery "Neue �bungsgruppe ausgew�hlt." $ 
		do	
		table $ do
			attr "width" "600"
			hrrow
			ttxt $ "Gruppe " ++ desc ++" ausgew�hlt."
			hrrow
			smallSubButton F0 (studStatusPage mat) "Weiter"	



-- Vorlesung entfernen
delVorlesungPage mat vorl F0 = do
	-- Vorlesungen mit bepunkteten Aufg. nicht l�schen
	vls <- io $ getVorlesungWithPointsDB mat 
	standardQuery "Vorlesung entfernen" $ 
		let 
			mglVl = [ v | v <- vorl , not ( v `elem` vls ) ]
		in	
		do
		table $ do 
			attr "width" "600"
			hrrow
			ttxt "Hinweis: Vorlesungen mit bepunkteten "
			ttxt "Aufgaben k�nnen nicht entfernt werden."
			
			if length mglVl > 0 
			 then
			    do 
				tr $ td $ text "Bitte w�hlen Sie eine Vorlesung aus."
				addVlF <- tr $ td $ selectSingle show Nothing mglVl empty
				hrrow
				smallSubButton (F1 addVlF) (deledVorlesungPage mat mglVl) "Entfernen"
				return ()
			 else
			   do
			   tr $ td $ text "Sie k�nnen keine Vorlesungen entfernen."
			   hrrow
			smallSubButton F0 (studStatusPage mat) "Zur�ck"

deledVorlesungPage mat mglVl (F1 addVlF) = do 
	let addVl = value $ addVlF 
	io $ removeStudVorlDB mat addVl
	standardQuery "Vorlesung entfernt." $ 
		do	
		table $ do
			attr "width" "600"
			hrrow
			tr $ td $ text $ "Vorlesung " ++ addVl ++ " entfernt."
			hrrow
			smallSubButton F0 (studStatusPage mat) "Weiter"	

-- Gruppen add

-- Email - �nderung
changeEmailPage mat email F0 =
	standardQuery "�nderung der Email-Adresse" $ do
		table $ do 
			attr "width" "600"
			hrrow
			ttxt $ "Mit einer �nderung muss Ihre Email-Adresse erneut �berpr�ft werden. " ++
					 "Sie k�nnen bis zur vollst�ndigen Pr�fung keine Emails an das Autotool senden."
			msgKontrollEmail
			emailF <- promptedInput "Neue Email-Adresse" (fieldVALUE email)
			hrrow
			smallSubButton (F1 emailF) (changedEmailPage mat email) "�ndern"  
			smallSubButton F0 (studStatusPage mat) "Zur�ck"
	
changedEmailPage mat oldemail (F1 emailF)	= do 
	let email = unEmailAddress $ value emailF
	do
	 if (oldemail == email) 
	   then do
			standardQuery "Fehler: Keine �nderung" $ do
				table $ do
					attr "width" "600"
					hrrow
					ttxt "Die Email-Adresse wurde nicht ge�ndert."
					hrrow
					smallSubButton F0 (studStatusPage mat) "Weiter"
		else do		
			io $ updateEmailDB mat email
			standardQuery "email" $ do
				table $ do 
					attr "width" "600"
					hrrow
					tableRow2	( text "Email-Adresse ge�ndert auf ") (text email)
					hrrow
					smallSubButton F0 (studStatusPage mat) "Weiter"

-- Passwort - �nderung
changePasswortPage mat F0 =
	standardQuery "�nderung des Passwort" $ do
		table $ do 
			attr "width" "600"
			hrrow
			msgPasswort
			ttxt ""
			ttxt "Bitte geben Sie Ihr neues Passwort zweimal ein:"	
			ttxt ""
			passwortF <- promptedPassword "Neues Passwort" (fieldSIZE 40)
			passwort2F <- promptedPassword "Wiederholung" (fieldSIZE 40)
			hrrow
			smallSubButton (F2 passwortF passwort2F) (changedPasswortPage mat) "�ndern"  
			smallSubButton F0 (studStatusPage mat) "Zur�ck"
	

changedPasswortPage mat (F2 passwortF passwort2F) = 
	let 
		passwort  = unNonEmpty $ value passwortF
		passwort2 = unNonEmpty $ value passwort2F
	in	
	  if (passwort2 == passwort) 
		then do		
			 io $ updatePasswortDB mat passwort
			 standardQuery "Passwort" $ do
				table $ do 
					attr "width" "600"
					hrrow
					ttxt "Passwort ge�ndert."
					hrrow
					smallSubButton F0 (studStatusPage mat) "Weiter"
		else do
			standardQuery "Fehler: " $ do
				table $ do
					attr "width" "600"
					hrrow
					ttxt "Passwort und Wiederholung nicht identisch."
					hrrow
					smallSubButton F0 (studStatusPage mat) "Weiter"
		 






