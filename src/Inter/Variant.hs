module Inter.Variant where

-- $Id$

import Inter.Types
import Inter.Error
import qualified Inter.Param as P

import qualified Passwort
import qualified Reporter 
import Reporter.Wash
import Size

import qualified Challenger 

import ToDoc
import Reader
import ParsecError
import ParsecPos

import qualified Posix

-- wash
import CGI hiding ( span, map, div, head, name )
import qualified CGI

-- control
import HTMLshortcuts 
import SQLqueries
import Helper

import Maybe (isJust)
import Monad ( guard )

interface :: [ Variant ] -> IO ()
interface vs = do
    user <- Posix.getEffectiveUserName
    Posix.setEnvVar "HOME" user
    CGI.run [] $ login ( P.empty { P.variants = vs } ) 

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
	let par = par0 { P.matrikel = value matF 
				   , P.passwort = value pwdF
				   , P.problem  = value proF
				   , P.variant  = value varF
				   , P.input    = ""
				   }
	in do  
	   mbsnr <- io $ loginDB (P.matrikel par) (show (P.passwort par))
	   case mbsnr of 
			Nothing  -> standardQuery "Login" $ CGI.text $ "Fehlgeschlagen!"
			Just snr -> computer $ par { P.snr = snr }



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
handler par0  (Variant v ) =  do  
	-- mgl. Aufgaben aus DB für snr aus DB
	-- und Aufgaben Number ermittlen
  mglAufgaben <- io $ mglAufgabenDB (P.snr par0)
  let anr = [ a | (a,name,subj,_,_) <- mglAufgaben
			, (P.problem par0) == name 
			, (P.variant par0) == subj
			]
  if null anr then standardQuery "Fehler" $ CGI.text "keine Variante des Students"
   else 
	standardQuery "Computer" $ do
    hr CGI.empty
    preface par0

    -- TODO: die folgenden Zeilen mit Cache !
    k <- lift $ unsafe_io $ key v ( P.matrikel par0 )
    inst <- lift $ unsafe_io $ washer $ gen v ( P.matrikel par0 )

    hr CGI.empty
    h2 $ CGI.text $ "Die Aufgaben-Instanz: Nr." ++ anr!!0
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
            pc <- CGI.p $ Reporter.Wash.embed $ Challenger.partial p i b
            h3 $ CGI.text "totale Korrektheit"
            tc <- CGI.p $ Reporter.Wash.embed $ Challenger.total   p i b
            h3 $ CGI.text $ "Aufgabe gelöst?"
	    if ( isJust pc && isJust tc )
	       then do
	          CGI.p $ CGI.text $ "Ja."
		  let s = size b
		  CGI.p $ CGI.text $ "Größe der Lösung: " ++ show s
		  -- hier (OK s) in DB eintragen TODO: ermittle Low/High von DB
		  -- $ io $ bepunkteStudentDB (P.snr par0) (anr!!0) (Helper.Ok s) Helper.Low
	       else do
	          CGI.p $ CGI.text "Nein."
		  -- hier (NO) in DB eintragen
		  --io $ bepunkteStudentDB (P.snr par0) (anr!!0) (Helper.No) Helper.Low
        Left e -> do
	    h3 $ CGI.text "Syntaxfehler"
	    CGI.pre $ CGI.text $ render $ errmsg e $ P.input par

    hr CGI.empty

---------------------------------------------------------------------------

errmsg :: ParseError -> String -> Doc
errmsg e inp = 
    let css = lines inp
	p = errorPos e
	(pre, post) = splitAt (sourceLine p - 2) css
	it  = replicate (sourceColumn p - 1) '-' ++ "?"
    in  vcat $ map ToDoc.text 
	     $ pre ++ [ it ]
		   ++ [ showErrorMessages "or" "unknown parse error" 
                              "expecting" "unexpected" "end of input"
                       (errorMessages e) ]
		   ++ [ it ]
		   ++ post


    




