module Inter.Variant where

-- $Id$

import Inter.Types
import Inter.Error
import qualified Inter.Param as P

import qualified Passwort
import qualified Reporter 
import Reporter.Washer

import qualified Challenger 

import Reader


-- wash
import CGI hiding ( span, map, div, head, name )
import qualified CGI

-- control
import HTMLshortcuts 

import Maybe (isJust)
import Monad ( guard )

interface :: [ Variant ] -> IO ()
interface vs = CGI.run [] $ login ( P.empty { P.variants = vs } ) 

login :: P.Type -> CGI ()
login par = standardQuery "Login" $ preface par


preface :: P.Type -> WithHTML CGI ()
-- das gibts beim ersten aufruf
preface par = do
    h2 $ CGI.text "Einstellungen:"
    makeForm $ table $ tr $ do
        (matF, pwdF) <- td $ table $ do
            matF <- texti "Matrikelnr:" 
        	    $ (fieldSIZE 30) ## ( fieldVALUE $ P.matrikel par )
            pwdF <- promptedPassword "Passwort:" 
        	    $ (fieldSIZE 30) ## ( fieldVALUE $ P.passwort par )
	    return (matF, pwdF)
        (proF, varF) <- td $ table $ do
            proF <- texti "Aufgabentyp:" 
        	    $ (fieldSIZE 10) ## ( fieldVALUE $ P.problem par )
            varF <- texti "Variante:" 
        	    $ (fieldSIZE 10) ## ( fieldVALUE $ P.variant par )
	    return (proF, varF)
	let args = F4 matF pwdF proF varF 
        td $ ssb args (settings par) "Login/Change"

texti :: String 
      ->  WithHTML CGI ()
      -> WithHTML CGI (InputField String INVALID) 
texti txt attrs = tr $ do 
      td $ CGI.text txt 
      td $ textInputField attrs

----------------------------------------------------------------------

-- falls neue eingabe der lösung
evaluator par0 ( F1 txtF ) = 
    computer $ par0 { P.input = value txtF }
    
-- falls neue settings
settings par0 (F4 matF pwdF proF varF) = 
    computer $ par0 { P.matrikel = value matF
	       , P.passwort = value pwdF
	       , P.problem  = value proF
	       , P.variant  = value varF
	       , P.input    = ""
	       }

-- rechnet alles aus
computer par = 
    let
	vvs :: [ Variant ]
        vvs = do
	   vv <- P.variants par
	   case vv of 
	       Variant v -> do
	           guard $ P.problem par  == show ( Inter.Types.problem v )
	           guard $ P.variant par  ==        Inter.Types.variant v
	           return vv
    in case vvs of
         [ vv ] -> handler par vv 
         _      -> error_handler par $ preface par


-- wenn kontrolleur gefunden,
-- dann instanz und nullte lösung berechnen
handler par0  (Variant v ) =   standardQuery "Computer" $ do

    hr CGI.empty
    preface par0

    -- TODO: die folgenden Zeilen mit Cache !
    k <- lift $ unsafe_io $ key v ( P.matrikel par0 )
    inst <- lift $ unsafe_io $ washer $ gen v ( P.matrikel par0 )

    hr CGI.empty
    h2 $ CGI.text "Die Aufgaben-Instanz:"
    Just i <- inst

    let p = Inter.Types.problem v

    let par = if null $ P.input par0
	      then par0 { P.input = show $ Challenger.initial p i }
	      else par0

    hr CGI.empty
    h2 $ CGI.text "neuer Lösungs-Versuch"    
    makeForm $ table $ tr $ do
        let height = length $ filter ( == '\n' ) $ P.input par
	txtF <- td $ makeTextarea ( P.input par ) 
		   $  attr "rows" ( show $ height + 2 )
		   ## attr "cols" "60"
        td $ ssb ( F1 txtF ) ( evaluator par ) "Compute"

    hr CGI.empty
    h2 $ CGI.text "voriger Lösungs-Versuch"
    CGI.pre $ CGI.text $ P.input par

    case parse reader "input"$ P.input par of
        Right b -> do
            h3 $ CGI.text "partielle Korrektheit"
            pc <- CGI.p $ Reporter.embed $ Challenger.partial p i b
            h3 $ CGI.text "totale Korrektheit"
            tc <- CGI.p $ Reporter.embed $ Challenger.total   p i b
            h3 $ CGI.text $ "Aufgabe gelöst? " 
		   ++ show ( isJust pc && isJust tc )
        Left e -> do
	    h3 $ CGI.text "Syntaxfehler"
	    CGI.pre $ CGI.text $ show e

    hr CGI.empty




    




