module Main where

import IO
import Wash.HTMLMonad 
import Wash.CGI
import Data.Char( toLower )

-- autoan-modules
import HTMLshortcuts
import SQLqueries
import Helper
import Exception
-- EX
import Database.MySQL.HSQL 
--  
-- TODO 
-- SQL-Exception fangen
-- Seiten Struktur rausziehen: stdpage ttl bdy menu
main :: IO ()
main = 
	run [] (
		--findStudPage 
		loginPage "" F0 --mainCGI 
		   )
  `Exception.catch` \ ex -> putStrLn $ "\n\n" ++ show ex

-- Einstieg: loginPage 
loginPage lgn F0  = 
	standardQuery "Willkommen zum Autotool-Admin." $ do 
		table $ do 
			attr "width" "600"
			hrrow
			lgnF <- promptedInput		"Name:"	$ (fieldSIZE 30) ## ( fieldVALUE lgn )
			pwdF <- promptedPassword	"Passwort:"		(fieldSIZE 30)
			hrrow
			smallSubButton (F2 lgnF pwdF)	checkLoginPage	"Login" 

-- TODO admin id muss durch gereicht werden, um verschieden rechte zuzulassen...."
-- z.B. Korrektor
checkLoginPage (F2 lgnF pwdF) = 
  let 
		lgn  = unNonEmpty ( value lgnF )
		pass = unNonEmpty ( value pwdF )
  in
  do 
--  lgnok <- io $ checkAdminNamePasswortDB lgn passwort
-- EX
  lgnok <-  io $ failDB lgn pass `catchSql` \e -> do 
		   error $ "sql fehler!" ++ (seErrorMsg e) 
  if lgnok 
	 then findStudPage
	 else standardQuery "Fehler" $ do
			hrline
			h3 $ text "Name oder Passwort fehlerhaft."
			hrline
			submit F0 (loginPage lgn) (fieldVALUE "Zurück")

-- 	do inh <- io $ checkPasswdMNrDB (Just pass) lgn
-- 	   if  ( (length inh)  == 0  ) -- passwort nicht okay?
-- 		 then 
-- 			do standardQuery "Fehler:" $ 
-- 				table $ do
-- 					hrrow		
-- 					th3 "Passwort und Matrikelnummer stimmen nicht überein!" 
-- 					hrrow
-- 					smallSubButton F0 (loginPage lgn) "Login wiederholen"
--  					smallSubButton F0 endPage		 "Ende"

-- 		 else 
-- 			do 


--
findStudPage = do 
	standardQuery "Auto-Admin" $ 
		do 
		findStudTable "" "" "" "" ""

findStudTable  vnm nme mat eml vrl = do
		table $ do	
		th3  "Suche nach Teilworten (und verknüpft)"	
		ttxt "Pattern \'_\' = ein Zeichen \'#\' = bel. viele Zeichen"
		spacerow
		matF <- promptedInput "Matrikelnr.:"	( (fieldSIZE 30) ## (fieldVALUE mat) ) 
		vnmF <- promptedInput "Vorname:"		( (fieldSIZE 30) ## (fieldVALUE vnm) )
		nmeF <- promptedInput "Name:"			( (fieldSIZE 30) ## (fieldVALUE nme) )
		emlF <- promptedInput "Email:"			( (fieldSIZE 30) ## (fieldVALUE eml) )
		vrlF <- promptedInput "Vorlesung:"		( (fieldSIZE 30) ## (fieldVALUE vrl) )
		tr $ 
		   do 
		   td $ submit (F5 vnmF nmeF matF emlF vrlF) foundStudPage (fieldVALUE "Suche")

foundStudPage (F5 vnmF nmeF matF emlF vrlF) =
	let	vnm = unText $ value vnmF
		nme = unText $ value nmeF
		mat = unText $ value matF
		eml = unText $ value emlF
		vrl = unText $ value vrlF
	in
	do
		erg <- io $ findStudDB vnm nme mat eml vrl
		standardQuery "Suchergebnis:" $ do
			hrline
			findStudTable vnm nme mat eml vrl
			hrline
			table $ do
				attr "border" "1"
				attr "frame" "void"
				attr "rules" "rows"
 				attr "cellspacing" "2" 
				attr "cellpadding" "5"
				sel <- radioGroup empty 
				-- Edit Menu
				tr $ do
					 td $ submit (F1 sel) (editStudentPage')  (fieldVALUE "Edt")
					 td $ submit (F1 sel) removeStudentPage (fieldVALUE "Del")
				-- Head [ "Sel." , Table-Head ... ]
				let thtxt x = th ( text x  ## attr "align" "left")							   
				tr $ th (text "Sel.") >> mapM_ thtxt ( fst erg )  
				-- Data [ radioButton , Table-Data ... ]
				let  { sline xs = tr $  
					   ( td (radioButton sel (tos xs) empty ) ) ## 
						( sequence $ Prelude.map textOrInt' xs )
					 ; textOrInt' (I i ) = td $ text (show i)
					 ; textOrInt' (S s ) = td $ text s
					 ; textOrInt2Str (I i) = show i;
					 ; textOrInt2Str (S s) = s;
					 ; tos xs = Prelude.map textOrInt2Str xs
					 }
				mapM_ sline ( snd erg ) 
				radioError sel


editStudentPage' (F1 sel) =
	let 
		str :: [ String ]
		str = value sel
		mat = str!!1 
	in 
	editStudentPage mat F0
	

inputStudent mat vnm nme eml pas = 
	do 
	matF <- promptedInput "Matrikelnr.:"	( (fieldSIZE 30) ## (fieldVALUE mat) ) 
	vnmF <- promptedInput "Vorname:"		( (fieldSIZE 30) ## (fieldVALUE vnm) )
	nmeF <- promptedInput "Name:"			( (fieldSIZE 30) ## (fieldVALUE nme) )
	emlF <- promptedInput "Email:"			( (fieldSIZE 30) ## (fieldVALUE eml) )
	pasF <- promptedInput "Passwd:"			( (fieldSIZE 30) ## (fieldVALUE pas) )
	return (matF, vnmF , nmeF , emlF , pasF )

addVorlesungChoice mat mgladdVl = 
	do
	if length mgladdVl > 0 
	   then
	   tr $ do
			addVlF <- td  (selectSingle show Nothing mgladdVl empty) 
			td $ submit (F1 addVlF) (addedVorlesungPage mat mgladdVl) (fieldVALUE "Add")
	   else
	   do
	   ttxt "Add Vorlesung: Bereits alle Vorlesungen ausgewählt."

removeVorlesungChoice mat mgldelVl = do 
	if length mgldelVl > 0	
	   then
	   tr $ do
			delVlF <- td (selectSingle show Nothing mgldelVl empty) 
			td $ submit (F1 delVlF) (deledVorlesungPage mat mgldelVl) (fieldVALUE "Del")
	   else do
	   ttxt "Del Vorlesung: Alle Vorlesungen des Studenten sind bepunktet. " 


editStudentPage mat F0 = 
	do 
	inh		<- io $ getStudentDB mat
	result	<- io $ studAufgDB mat
	-- besuchte, bepunktete und alle mgl. Vorlesungen 
	vl		<- io $ studVorlDB mat
	vlwpt	<- io $ getVorlesungWithPointsDB mat 
	vls		<- io $ getAllVorlesungenDB
	let 
		( S vnm , S nme , S eml , S sta , S pas ) = inh !! 0
		mgladdVl = [ v | v <- vls , not ( v `elem` vl ) ]
		mgldelVl = [ v | v <- vl  , not ( v `elem` vlwpt ) ]
	standardQuery "Edit Student" $ 
		table $ 
			do
			hrrow
			-- Stammdaten ändern
			(matF,vnmF,nmeF,emlF,pasF) <- inputStudent mat vnm nme eml pas
			submit	(F5 matF vnmF nmeF emlF pasF) 
					(editSubmitStudentPage mat vnm nme eml pas) 
					(fieldVALUE "Submit")
			hrrow
			-- Vorlesungen
			tableRow2 (text "Vorlesung:") $ text $ if length vl > 0 then foldr1 kommas vl else "keine" 
			-- Vorlesungen hinzufügen (bereits verhanden ignorieren)
			addVorlesungChoice mat mgladdVl
			-- Vorlesung entfernen (bepunktete ignorien)			  
			removeVorlesungChoice mat mgldelVl
			-- Ergebnisse des Studenten
			hrrow
			smallspacerow
			if length (snd result) > 0 
			   then do { h3 $ text "Ergebnisse" ; ( showAsTable result ) }
			   else do { h3 $ text "keine Ergebnisse" } 
			hrrow


editSubmitStudentPage  mat' vnm' nme' eml' pas' (F5 matF vnmF nmeF emlF pasF) =
	let 
		mat		= unAllDigits	$ value matF
		vnm		= unNonEmpty	$ value vnmF
		nme		= unNonEmpty	$ value nmeF
		eml		= unNonEmpty	$ value emlF
		pas		= unText		$ value pasF
		matok	= if allDigits mat then (read mat) > 1024 else False
	in 
	do 
	-- suche nach dupletten von email or mat
	( matDup , emlDup ) <- io $ duplMatOrEmailDB mat eml
	-- [(was kann schief gehen , fehlermsg )]
	let { checks = [ ( not matok						
					 , "Die Matrikelnummer muss größer 1024 sein.")
				   , ( length vnm < 2					
					 , "Der Vorname muss mindestens 2 Buchstaben lang sein.") 
				   , ( length nme <	2					
					 , "Der Name muss mindestens 2 Buchstaben lang sein.") 
				   , ( matDup && (mat' /=  mat)			
					 , "Matrikelnummer bereits registiert." )
				   , ( emlDup && (eml' /= eml)			
					 , "Email bereits registiert." )
				   ]
	-- alles ok?
		; allok = and [ not (fst c) | c <- checks ]
		}
	-- alles ok -> abfeuern
	if allok 
	   then io $ updateStudDB mat' mat vnm nme eml pas	   
	   else return ()
	standardQuery "Hello" $ table $ 
		do 
		if allok 
		   then ttxt "Alles Okay"
		   else mapM_ ttxt [  msg | ( ok , msg ) <- checks , ok ]


addedVorlesungPage mat mglVl (F1 addVlF) = do 
	let addVl = value $ addVlF 
	io $ insertStudVorlDB mat addVl
	standardQuery "Vorlesung hinzufügen" $ 
		do	
		table $ do
			attr "width" "600"
			hrrow
			tr $ td $ text $ "Vorlesung " ++ addVl ++ " hinzugefügt."
			hrrow
			smallSubButton F0 (editStudentPage mat) "Weiter"

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
			smallSubButton F0 (editStudentPage mat) "Weiter"	

removeStudentPage (F1 sel) = 
	let 
		str :: [String]
		str = value sel
	in
	standardQuery "Remove Student" $ 
				  table $ 
						do 
						h3 $ text "noch nicht fertig..."
						tr $ text $ show str
