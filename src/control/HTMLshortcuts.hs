module HTMLshortcuts where

--   $Id$

import Prelude hiding ( span, head, div, map )
import qualified Prelude

import qualified Passwort


import Wash.HTMLMonad 
import Wash.CGI

import IO
import Data.Char -- toLower
-- import Exception -- 

import Helper


--  ------------------------------------------------------------------------------
-- Helper InputFields
promptedInput txt attrs =
	tr $ do 
		 td $ text txt 
		 td $ inputField attrs

promptedPassword txt attrs =
	tr $ do 
		 td $ text txt 
		 td $ passwordInputField attrs

textOrInt (I i ) = td $ text (show i)
textOrInt (S s ) = td $ text s

-- Helper Button ([>] str)
smallSubButton subParm subAktion str =
-- 	smallSubButton2 subParm subAktion str (ttxt "")
	tr $ td $ (attr "colspan" "2") >> ssb subParm subAktion str 

ssb subParm subAktion str = 
   ( ( submit subParm subAktion (fieldVALUE ">>")) >> text ( "  " ++ str ) )

-- smallSubButton2 subParm subAktion str tdd = 
-- 	tr $ td td1 >> tdd
-- 		where td1 = ( submit subParm subAktion (fieldVALUE ">>")) >> text ( "  " ++ str )
smallSubButton2 subParm subAktion str tdd = 
	tr $ do 
		 td tdd 
		 td1
		 where td1 = ( submit subParm subAktion (fieldVALUE ">>")) >> text ( "  " ++ str )


-- 
showAsTable :: ( [ String ] , [ [ StrOrInt ] ]) -> WithHTML CGI ()
showAsTable inh = 
	table $ do 
--		attr "border" "1"
 		attr "cellspacing" "2" 
		attr "cellpadding" "5"
		tr $ mapM_ ( \ x -> ( th ( text x  ## attr "align" "left") ) ) ( fst inh )  
		sline ( snd inh )
		where
--			sline xs = tr $ sequence $ Prelude.map textOrInt xs 
			row' x = tr $ sequence $ Prelude.map textOrInt x
			sline [] = empty
			sline (x:[]) = row' x >> attr "bgcolor" "white" >> empty
			sline (x:y:xs) = row' x >> attr "bgcolor" "black" >> ( row' y >> attr "bgcolor" "white") >> sline xs

showAsTable2 :: ( [ String ] , [ [String] ]) -> WithHTML CGI ()
showAsTable2 inh = 
	table $ do 
		attr "border" "1"
 		attr "cellspacing" "2" 
		attr "cellpadding" "5"
		tr $ mapM_ ( \ x -> ( th ( text x  ## attr "align" "left") ) ) ( fst inh )  
		mapM_ sline ( snd inh )
		where
			sline xs = tr $ sequence $ Prelude.map (\s -> td $ text s) xs 




-- table row shortcuts
tableRow2 d1 d2 =
	tr $ ( td d1 ) >> ( td d2 )  

hrline :: WithHTML CGI ()
hrline = table $ (attr "width" "600" ) >> tr ( td ( hr empty ) )

hrrow :: WithHTML CGI ()
hrrow  = tr $ td $ (attr "colspan" "2") >> ( hr empty ) 

spacerow' :: String -> WithHTML CGI ()
spacerow' height = tr $ td $ (attr "colspan" "2") >> (attr "height" height) >> ( text " " ) 

spacerow = spacerow' "15"
smallspacerow = spacerow' "5"

ttxt :: String -> WithHTML CGI ()
ttxt s = tr $ td $ (attr "colspan" "2") >> ( text s ) 
th3 :: String -> WithHTML CGI ()
th3 s = tr $ td $ (attr "colspan" "2") >> ( h3 (text s ) )

{-
newtype MatrikelNr = MatrikelNr { unMatrikelNr :: String }

instance Read MatrikelNr where
	readsPrec i str = let str' = 
		if ok then [(MatrikelNr str' , dropWhile isDigit str )]
-}


