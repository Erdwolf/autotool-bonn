module Helper where

--   $Id$

import Prelude
import qualified Prelude ( map )

import Char ( toLower )
import Control.Monad ( mzero )


data StrOrInt = S String | I Int deriving ( Show , Read )


data ATBepunkte = ATBEmpty | ATB String String ATBewertung ATHighLow
		  deriving ( Show, Read )


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
