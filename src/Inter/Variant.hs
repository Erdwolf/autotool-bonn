module Inter.Variant where

-- $Id$

import Inter.Types
import qualified Inter.Param as P

import qualified Passwort
import qualified Reporter 
import Reporter.Washer

import qualified Challenger 



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
    h2 $ CGI.text "Einstellungen"
    makeForm $ table $ tr $ do
        args <- td $ table $ do
            matF <- texti "Matrikelnr:" 
        	    $ (fieldSIZE 30) ## ( fieldVALUE $ P.matrikel par )
            pwdF <- promptedPassword "Passwort:" 
        	    $ (fieldSIZE 30) ## ( fieldVALUE $ P.passwort par )
            proF <- texti "Aufgabentyp:" 
        	    $ (fieldSIZE 30) ## ( fieldVALUE $ P.problem par )
            varF <- texti "Variante:" 
        	    $ (fieldSIZE 30) ## ( fieldVALUE $ P.variant par )
            return $ F4 matF pwdF proF varF
        td $ ssb args (settings par) "Login/Change"
    return ()

texti :: String 
      ->  WithHTML CGI ()
      -> WithHTML CGI (InputField String INVALID) 
texti txt attrs = tr $ do 
      td $ CGI.text txt 
      td $ textInputField attrs

----------------------------------------------------------------------

evaluator par0 ( F1 txtF ) = 
    computer $ par0 { P.input = value txtF }
    

settings par0 (F4 matF pwdF proF varF) = 
    computer $ par0 { P.matrikel = value matF
	       , P.passwort = value pwdF
	       , P.problem  = value proF
	       , P.variant  = value varF
	       , P.input    = ""
	       }

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
         _      -> error_handler par 


error_handler par = standardQuery "Unbekannte Aufgabe" $ do
    table $ do
	tr $ td $ CGI.text $ "Die Aufgabe/Variante"
	tr $ td $ CGI.text $ P.problem par ++ "/" ++ P.variant par
	tr $ td $ CGI.text $ "ist nicht bekannt oder nicht aktiviert."
    preface par    

-- wenn kontrolleur gefunden,
-- dann instanz und nullte lösung berechnen
handler par0  (Variant v ) =   standardQuery "Computer" $ do

    preface par0

    -- TODO: die folgenden Zeilen mit Cache !
    k <- lift $ unsafe_io $ key v ( P.matrikel par0 )
    inst <- lift $ unsafe_io $ washer $ gen v ( P.matrikel par0 )

    h2 $ CGI.text "Die Aufgaben-Instanz:"
    Just i <- inst

    let p = Inter.Types.problem v

    let par = if null $ P.input par0
	      then par0 { P.input = show $ Challenger.initial p i }
	      else par0

    h2 $ CGI.text "neuer Lösungs-Versuch"    
    makeForm $ do
	txtF <- makeTextarea ( P.input par ) CGI.empty
        ssb ( F1 txtF ) ( evaluator par ) "Compute"

    h2 $ CGI.text "voriger Lösungs-Versuch"
    CGI.pre $ CGI.text $ P.input par
    let b = read $ P.input par

    h3 $ CGI.text "partielle Korrektheit"
    pc <- CGI.p $ Reporter.embed $ Challenger.partial p i b
    h3 $ CGI.text "totale Korrektheit"
    tc <- CGI.p $ Reporter.embed $ Challenger.total   p i b

    h3 $ CGI.text $ "Aufgabe gelöst? " ++ show ( isJust pc && isJust tc )



    




