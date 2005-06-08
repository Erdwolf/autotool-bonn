-- |
-- Autor:	Alf Richter
-- File: 	Schriftl.hs
-- Funktion: 	halbautomatische Erstellung einer 
-- 		serienweisen Punkteuebersicht

-- (0)   Zuordnung von Serie -> Autotoolsubject
--	'serie i :: Serie -> SQL-REGEXP'

-- (1) 	Serienweise Extraktion der Autotoolpunkte
--	'getAutoPunkteDB i :: Serie -> [(mnr,autotool-punkte)]'

--  EXAMPLE SESSION FUER SERIE 7

--  Autotool-Punkte

-- >	cd ~/autotool/control
-- >	make ghci-schriftl
-- > 	- Regexp serie 7 = [(...,...)...] anlegen
-- > 	p <- getAutoPunkteDB 7 		-- punkte passen zum regexp aus db holen
-- > 	print p 			-- ausgabe zur kontrolle
-- > 	foldr1 min $ [ x | (_,x) <- p ] -- maximum der punkte checken
-- > 	foldr1 max $ [ x | (_,x) <- p ] -- minimum der punkte checken
-- > 	fillAutInDB [7] 		-- punkte in die punkte tabelle einspielen
-- >      ghci verlassen (ctrl-d)


--  Schriftl.-Punkte

-- >	cd ~/edu 
-- >      cvs update -d
-- >	cp ~/edu/ws03/as/serie7/serie7.dat ~/autotool/control/serie7-schriftl.dat
-- >	cd ~/autotool/control
-- >	make ghci-schriftl
-- >	ls <- opendat 7 	-- punkte laden
-- >	fndup ls 		-- sind duplikate verhanden? true -> abbruch !!!
-- >	print ls 		-- anzeigen
-- >	fillDatInDB [7]		-- punkte in punkte tabelle einspielen
-- >  ghci verlassen (ctrl-d)


--  Report erstellen

-- >	cd ~/autotool/control
-- >	make Schriftl.cgi
-- >	Schriftl.cgi > report.html
-- > 	perl -0777 -i~ -pe 's,<input.*?>,,sg' report.html  
-- >	links report.html


module Main where

import Prelude as P
import Char
import Data.List
import Numeric

import IO
import HTMLMonad
import CGI as C

import Database.MySQL.HSQL
import Mysqlconnect

import SQLqueries


-- ================================================================================
-- Schriftliche Punkte
-- Main
fillDatInDB xs = do
        let fill x = do ls <- opendat x 
			if not (fndup ls) 
			   then fillPunkteDB ("Serie " ++ show x ++ " schriftlich") ls
			   else return ()
	P.mapM_ fill xs

-- ================================================================================
-- Autotool Punkte
--  -
-- Main

--
-- Serien ss 2003  
--
--serie 1: TM-{SUCC,EXPO,STEP[12]} 4 pkte / schrift: TM-BIN 6 pkte
--serie 2: TM-FACTOR, LOOP-{TIME,SQRT,PRIM,FIB} 5 pkte / sch: 5 pkte
--serie 3: FUN-{SQRT,FIB,QUIZ} 3 pkte / sch: ACK 3, DPR 4 pkt
--serie 4: kein tool / sch: DIAG 5, ENT 5 pkt
--serie 5: {PCP,SAT}-QUIZ 2 pkt / sch: CFI 4, COL 4

--serie 1 = [("TM","SUCC|EXPO|STEP[12]|BIN")]
--serie 2 = [("TM","FACTOR"), ("LOOP","TIME|SQRT|PRIM|FIB")]
--serie 3 = [("FUN","SQRT|FIB|QUIZ")]
--serie 5 = [("PCP","QUIZ"),("SAT","QUIZ")]

--
-- Serien ws 2003
--
-- Serie 1: s-7 Punkte a-5 Punkte (Synthese: L-1 bis L-5)
-- Serie 2: s-7 Punkte a-4 Punkte (Synthese: L-6 und 7; Determine: D-1 
-- !ACHTUNG 2 Punkte fuer D1!)
-- Serie 3: s-8 Punkte a-6 Punkte (Synthese: S-Drei, Reg1 und Reg2; Analyse: A-Zwei,Drei und Aba)
-- Serie 4: s-7 Punkte a-5 Punkte (CFG: G-ComPali, Gleich, Dyck: Pump:P-Drei,Dyck)
-- Serie 5: s-6 Punkte a-5 Punkte (CFG:G-IJK,ComDyck: CFG:Ein-Dyck,Gleich; CFG:CNF-ComPali)
serie 1 = [("L","[12345]")]
serie 2 = [("L","[67]"), ("D","1")]
serie 3 = [("S","Drei|Reg1|Reg2"),("A","Zwei|Drei|Aba")]
serie 4 = [("G","ComPali|Gleich|Dyck"),("P","Drei|Dyck")]
serie 5 = [("G","IJK|ComDyck"),("Ein","Dyck|Gleich"),("CNF","ComPali")]
serie 6 = [("P","Gleich|Power2"),("CFG","QuizCNF"),("NPDA","OGleich|Dyck")]
serie 7 = [("P","ComPower2"),("NPDA","Gleich")
	  ,("NPDAdet","Dyck|Gleich"),("D","2"),("NPDA","Quiz")]



-----------------------------------------------------------------
-- VORSICHT HACK:  D-1 (anr=15) wird dpl. gezaehlt
-----------------------------------------------------------------

--
-- let match only whole words in sql-regexp
-- 
protectRegexp xs = [(x, "^(" ++ y ++ ")$") | (x,y) <- xs ]

-- make ghci-schrifl
-- > do { ls <- getPunkteDB 3 ;  print $ sort ls }
getAutoPunkteDB i = 
    do
    conn <- myconnect
    stat <- query conn $ sqlstr
    inh  <- collectRows (\ state -> 
			 do 
			 	m <- getFieldValue state "MNr"
			 	p <- getFieldValue state "Punkte"
			 	return ( m :: String , p :: String )
			) stat
    disconnect conn
    return inh
    where sqlstr = 
	      concat [ "SELECT s.MNr , \n"
-- HACK D1 wird dpl gezaehlt
--		     , "SUM(if(OK>0,1,0)) AS Punkte \n"
		     , "SUM(if(OK>0,if(sa.ANr=15,2,1),0)) AS Punkte \n"
		     , "FROM stud_aufg AS sa, student AS s, aufgabe AS a \n"
		     , "WHERE a.ANr = sa.ANr AND sa.SNr = s.SNr \n"
		     , "AND ( " , s i , " ) "
		     , "GROUP BY MNr \n"
		     , "ORDER BY MNr;"
		     ]
	-- zusammenbasteln
	  -- zB. ' ( a.Name ... ) OR ( a.Name ... ) ... OR ( a.Name )'
	  s 		= sor . sand
	  -- Matcher mit OR verkleben
	  sor 		= foldr1 sor' 
	  sor' a b	= a ++ " OR " ++ b
	  -- erzeuge Matcher
	  sand i 	= P.map f $ protectRegexp $ serie i
	  -- Matchter fuer uebereinstimmung von aufgabenname und subjekt-regexp 
	  -- zB. ' ( a.Name = "TM" AND a.Subject REGEXP "SUCC|EXPO|STEP[12]|BIN" ) '
	  f (a,b) 	= " ( " ++ "a.Name =  \"" ++ a ++ "\" AND " ++ "a.Subject REGEXP \"" ++ b ++ "\" " ++ " ) "


fillAutInDB xs = do
        let fill x = do ls <- getAutoPunkteDB x
			if not (fndup ls) 
			   then fillPunkteDB ("Serie " ++ show x ++ " autotool") ls
			   else return ()
	P.mapM_ fill xs

-- ================================================================================
-- HELPER

fillPunkteDB rubrik matpkts = do 
	conn <- myconnect
	-- sql: ein eintrag in punkte table
	let qustring (mat,pkt) = query conn ( concat
                          [ "INSERT INTO punkte \n"
                          , "(Rubrik,MNr,Punkte) \n"
                          , "VALUES ( "
                          , "\"" , rubrik , "\" , "
                          , "\"" , mat , "\" , "
                          , "\"" , pkt , "\" )"
                          , ";"
                          ] )
	-- fuer alle eintraege
	P.mapM_ qustring matpkts
	disconnect conn
	return ()


beforeSpace = takeWhile isAlphaNum
afterSpace  = takeWhile isAlphaNum . dropWhile isSpace . dropWhile isAlphaNum

splitAtSpace s = (b,a)
	where b = beforeSpace s	; a = afterSpace s

-- readDec' = fst . (!!0) . readDec
-- readDecSnd (a,b) = (a, readDec' b)

opendat :: Int -> IO [(String , String )]
opendat i = do fh <- readFile ( "serie" ++ show i ++ "-schriftl.dat" )
	       let ls = lines fh
	       let ls'= P.map splitAtSpace ls
	       return $ sort ls'

--
-- Enthaelt die Liste Duplikate?
--
-- Input: Liste
-- Ouput: True wenn ja , sonst False
--
fndup :: Ord a => [a] -> Bool
fndup = fndup' . sort
	where fndup' (x:y:xs) 	= x == y || fndup' (y:xs)
	      fndup'  x 	= False





fusion :: Ord a => [(a,a,a,a,a)] -> [(a,a,a,[(a,a)])]
fusion xs = fus ( P.map fusSP xs )
    where fusSP (n,v,m,s,p) = (n,v,m,[(s,p)])
	  fus ((n,v,m,x):(nn,vv,mm,xx):rest) =
	  -- alle serien und pkt studentenweise zusammenfassen
	      if and [ n == nn , v == vv ]
		 then fus ( ( n , v , m , x ++ xx ) : rest )
		 else (n,v,m,x) : fus ( (nn,vv,mm,xx) : rest )
	  fus x = x

sumsp xs = [ (sum' sp ,n ++ " , " ++ v ++ " , " ++ m, fss $ fs sp ) | (n,v,m,sp) <- xs ]
    where 
    -- gesamtpkte des studenten berechnen
    sum' :: [(String,String)] -> Int
    sum' = sum . P.map (read . snd)
    -- gleiche serien zusammenfuehren ("1a","0") ("1s","1") -> ("1as","0|1")
    fss (a@(x,x'):b@(y,y'):xs) = 
	if  x!!0 == y!!0 
	    then ([x!!0] ++ "as" , x' ++ "|" ++ y' ): fss xs
	    else a : fss (b:xs)
    fss x = x 
    -- unbewerte aufgaben auffuellen
    fs x  = sort $ (sp' x ) ++ (fehlen $ sp' x)
    sp' x = P.map ws x
    ws (a,b) = ([a!!6] ++ [a!!8],  b)
    -- alle mgl. serien
    as = [ [x]++[y] | x <- ['1'..'5'] , y <- ['a','s']]
    -- nicht bepktete serien
    fehlen x  = [ (a,"-") | a <- as , not $ elem a (P.map fst x)]

make = sort . sumsp . fusion
-- get = getAllSerienPunkteDB

-- showit = do { i <- getAllSerienPunkteDB ; let l = sort $ sumsp $ fusion i in P.mapM_ putStrLn $ P.map show l  }

flatten xs = [ [ show m , n , f k ] | (m,n,k) <- xs ]
	     where fld a b = a ++ ", " ++ b  
		   f x = P.foldr1 fld $ snd $ unzip x

--
-- liefert Serien Punkte von allen Studenten
--
getAllSerienPunkteDB2 :: IO [(String,String,String,String,String)]
getAllSerienPunkteDB2 =
    do
    conn <- myconnect
    stat <- query conn $ sqlstr
    inh  <- collectRows (\ state ->
                         do     
			 	n <- getFieldValue state "Name"
			 	v <- getFieldValue state "Vorname"
			 	m <- getFieldValue state "Mnr"
                                p <- getFieldValue state "Punkte"
                                s <- getFieldValue state "Serie"
                                return ( n :: String , v :: String, m :: String, s :: String , p :: String )
                        ) stat
    disconnect conn
    return inh

    where sqlstr =
              concat [ "SELECT \n" 
		     , "s.Name AS Name, \n"
		     , "s.Vorname AS Vorname, \n"  
		     , "s.Mnr AS Mnr, \n"  
		     , "p.punkte AS Punkte , \n"
                     , "p.rubrik AS Serie \n"
                     , "FROM punkte AS p , student AS s \n"
                     , "WHERE \n" 
		     , "s.MNr = p.MNr \n"
 		     , "AND s.MNr > 1024 \n"
                     , "ORDER BY s.Name, s.Vorname, Serie ;"
                     ]

--
--
-- Erzeugen der Gesamtuebersicht als Html Seite
--
--
main = do run [] startPage

startPage = 
    do
    d <- io $ getAllSerienPunkteDB2
    let daten = reverse $ flatten $ make d
    let maxpt = 78 --foldr1 max (getpts daten)
    let len = length daten


    standardQuery "Automaten und Sprachen WS2003" $ 
      do
      table $ 
              do 
                sline [ "Punkte", "Name,Vorname,MNr" ,"Punkte je Serie [1-5] autotool|schriftl." ]
                mapM_ sline daten
--      table $ do mapM_ sline $ P.map (\(a,b) -> [a,b]) $ cumsum $ zip (getpts daten) [1..]
      table $ do 
	      sline ["Punkte", "#Studenten mit mehr Punkten"]
	      mapM_ sline $ P.map unp1 $ cumsum $ zip (getpts daten) [1..]
      table $ do 
	      sline ["Punkte in %", "#Studenten mit mehr Punkten in %"]
	      mapM_ sline $ P.map (unp2 maxpt len) $ cumsum $ zip (getpts daten) [1..]

      where sline xs      = tr $ sequence $ P.map (\s -> td $ text s) xs
	    getpts xs 
		      = P.map (read.(!!0)) xs :: [ Int ] -- nur die punkte
	    cumsum (x:xs) = f x xs 
	    f a (b:xs)    = if (fst a) /= (fst b) then a : f b xs else f b xs ; f a [] = [a]
	    unp1 (a,b) = [show a, show b]
	    unp2 maxpt len (a,b) = [ show (proz a maxpt) ++ "%" 
				   , show (proz b len) ++ "%"]
	    proz x h = ( fromRational ((toRational x ) * ( 100.0 / ( toRational h ) )) ) ::Float
	    
