module Main where

import Prelude as P
import Char
import List
import Numeric


import IO
import HTMLMonad
import CGI as C

import Database.MySQL.HSQL

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
-- ---
-- Main

--serie 1: TM-{SUCC,EXPO,STEP[12]} 4 pkte / schrift: TM-BIN 6 pkte
--serie 2: TM-FACTOR, LOOP-{TIME,SQRT,PRIM,FIB} 5 pkte / sch: 5 pkte
--serie 3: FUN-{SQRT,FIB,QUIZ} 3 pkte / sch: ACK 3, DPR 4 pkt
--serie 4: kein tool / sch: DIAG 5, ENT 5 pkt
--serie 5: {PCP,SAT}-QUIZ 2 pkt / sch: CFI 4, COL 4

-- serie 1 = 
serie 1 = [("TM","SUCC|EXPO|STEP[12]|BIN")]
serie 2 = [("TM","FACTOR"), ("LOOP","TIME|SQRT|PRIM|FIB")]
serie 3 = [("FUN","SQRT|FIB|QUIZ")]
serie 5 = [("PCP","QUIZ"),("SAT","QUIZ")]


getPunkteDB i = 
    do
    conn <- myconnect
    stat <- query conn $ sqlstr
    inh  <- collectRows (\ state -> 
			 do 
			 	m <- getFieldValue state "MNr"
--			 	t <- getFieldValue state "Typ"
--			 	n <- getFieldValue state "Nr"
			 	p <- getFieldValue state "Punkte"
			 	return ( m :: String , p :: String )
			) stat
    return inh
    where sqlstr = 
	      concat [ "SELECT s.MNr , \n"
		     , "SUM(if(OK>0,1,0)) AS Punkte \n"
		     , "FROM stud_aufg AS sa, student AS s, aufgabe AS a \n"
		     , "WHERE a.ANr = sa.ANr AND sa.SNr = s.SNr \n"
		     , "AND ( " , s i , " ) "
		     , "GROUP BY MNr \n"
		     , "ORDER BY MNr;"
		     ]
	  s = sor . sand
	  sor 	= foldr1 sor' 
	  sor' a b	= a ++ " OR " ++ b
	  sand i 	= P.map f $ serie i
	  f (a,b) 	= "a.Name =  \"" ++ a ++ "\" AND " ++ "a.Subject REGEXP \"" ++ b ++ "\" "


fillAutInDB xs = do
        let fill x = do ls <- getPunkteDB x
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

-- Enthaelt die liste Duplikate?
-- Input: Liste
-- Ouput: True wenn ja , sonst False
fndup :: Ord a => [a] -> Bool
fndup = fndup' . sort
	where fndup' (x:y:xs) 	= x == y || fndup' (y:xs)
	      fndup'  x 	= False





fusion :: Ord a => [(a,a,a,a)] -> [(a,a,[(a,a)])]
fusion xs = fus ( P.map fusSP xs )
    where fusSP (n,v,s,p) = (n,v,[(s,p)])
	  fus ((n,v,x):(nn,vv,xx):rest) =
	  -- alle serien und pkt studentenweise zusammenfassen
	      if and [ n == nn , v == vv ]
		 then fus ( ( n , v , x ++ xx ) : rest )
		 else (n,v,x) : fus ( (nn,vv,xx) : rest )
	  fus x = x

sumsp xs = [ (sum' sp ,n ++ " , " ++ v , fss $ fs sp ) | (n,v,sp) <- xs ]
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


-- liefert Serien Punkte von allen Studenten
--
getAllSerienPunkteDB2 :: IO [(String,String,String,String)]
getAllSerienPunkteDB2 =
    do
    conn <- myconnect
    stat <- query conn $ sqlstr
    inh  <- collectRows (\ state ->
                         do     
			 	n <- getFieldValue state "Name"
			 	v <- getFieldValue state "Vorname"
                                p <- getFieldValue state "Punkte"
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



main = do run [] startPage

startPage = 
    do
    d <- io $ getAllSerienPunkteDB2
    let daten = reverse $ flatten $ make d
    standardQuery "Berechenbarkeit und Komplexit SS2003" $ 
	table $ 
	      do 
		sline [ "Punkte", "Name,Vorname" ,"Punkte je Serie [1-5] autotool|schriftl." ]
		mapM_ sline daten
    where sline xs = tr $ sequence $ P.map (\s -> td $ text s) xs

			     
