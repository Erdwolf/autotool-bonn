module Helper where

-- $Id$

import Prelude
import qualified Prelude ( map )

import Char ( toLower )
import Monad ( mzero )


data StrOrInt = S String | I Int deriving ( Show , Read )

data ATBewertung = No | Ok Int deriving ( Show, Read )

data ATHighLow = High | Low | Keine 
instance Show ATHighLow where
    show High = "high" ; show Low = "low" ; show Keine = "keine"
instance Read ATHighLow where
    readsPrec p cs = do
       ( this, rest ) <- lex cs
       it <- case map toLower this of
            "high" -> return High
            "low"  -> return Low
            "keine" -> return Keine
            _ -> mzero
       return ( it, rest )


space a b	= a ++ " " ++ b

kommas a b = a ++ ", " ++ b 

allDigits x = and $ Prelude.map (`elem` ['0'..'9']) x  

-- nur buchstaben,zahlen [@-_.] erlauben
-- SQL sch�tzen vor injekted sql angriff
filterQuots :: String -> String
filterQuots s = [ c | c <- s 
				,  (toLower c ) `elem` ( ['a'..'z'] ++ "���� " ++ ['0'..'9'] ++ "@-_." ) 
				]

-- f�r passworter d�rfen wir nicht so streng sein...
-- TODO muss noch mehr gesch�tzt werden?
quoteQuots :: String -> String
quoteQuots [] = []
quoteQuots (x:xs) = y ++ quoteQuots xs
	where 
	y = if x `elem` ['\'','\"','\\','`'] 
		then '\\':(x:[])
		else x:[]

showFreeDB i =
    mapM_ putStrLn 
	      [ (show s) ++ "/" ++(show m) ++ " "++(show gnr) ++ " " ++ g ++ " " ++ n ++ " "++ r
		| (gnr,[g,n,r,m,s])<- snd i]