module Inter.Variant {-# DEPRECATED #-}

where

--   $Id$

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
import Control.Monad ( guard )

input_width :: Int
-- for textarea
input_width = 70 
-- height wird automatisch berechnet

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
    {- makeForm $ -} 
    table $ tr $ do
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

evaluator par0 bep@(ATB snr anr bew hl) ( F1 txtF ) = do
    io $ bepunkteStudentDB snr anr bew hl
    computer (par0 { P.input = value txtF }) bep
    
evaluator par0 _ (F1 _)= 
    standardQuery "Fehler" $ CGI.text "evaluator"    

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
			Just snr -> computer (par { P.snr = snr }) ATBEmpty



-- rechnet alles aus
computer par bep = 
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
         [ vv ] -> handler par vv bep
         _      -> error_handler par $ preface par


-- wenn kontrolleur gefunden,
-- dann instanz und nullte lösung berechnen
handler par0  (Variant v ) bep=  do  
	-- mgl. Aufgaben aus DB für snr aus DB
	-- und Aufgaben Number ermittlen
  mglAufgaben <- io $ mglAufgabenDB (P.snr par0)
  let ahs = [ (a,hl) | (a,name,subj,_,hl) <- mglAufgaben
			, (P.problem par0) == name 
			, (P.variant par0) == subj
			]

  k    <- io $ key v ( P.matrikel par0 )
  -- inst <- io $ washer $ gen v ( P.matrikel par0 )
  let i = gen_i v ( P.matrikel par0 )



  if null ahs 
   then standardQuery "Fehler" $ CGI.text "keine Variante des Students"
   else standardQuery "Computer" $ do
    let { [(anr,hlstr)] = ahs ; snr = P.snr par0 
	} 
    hr CGI.empty
    preface par0

    -- Zeige Ergebnisse dieser Aufgabe
    ergtabl @ (header , ergs) <- lift $ unsafe_io $ studAufgDB (P.matrikel par0)
    let erg = [ e | e@[_,S  t,S s,_,_] <- ergs
	      , (P.problem par0) == t
	      , (P.variant par0) == s
	      ]
    
    -- TODO: die folgenden Zeilen mit Cache !

    hr CGI.empty
    h2 $ CGI.text $ "Die Aufgaben-Instanz: Nr." ++ anr

    CGI.pre $ CGI.text $ show i

    let p = Inter.Types.problem v

    let par = if null $ P.input par0
	      then par0 { P.input = show $ Challenger.initial p i }
	      else par0
    if null erg then CGI.empty else showAsTable (header,erg)
    CGI.text $ show bep
    hr CGI.empty
    h2 $ CGI.text "neuer Lösungs-Versuch"    
    txtF <- table $ tr $ do
        let height = length $ filter ( == '\n' ) $ P.input par
	txtF <- td $ makeTextarea ( P.input par ) 
		   $  attr "rows" ( show $ height + 2 )
		   ## attr "cols" ( show input_width )
	return txtF
    --        td $ ssb ( F1 txtF ) ( evaluator par ) "Compute"
    hr CGI.empty
    h2 $ CGI.text "voriger Lösungs-Versuch"

    bep <- case parse reader "input" $ P.input par of
        Right b -> do
	    CGI.pre $ CGI.text $ P.input par
            h3 $ CGI.text "partielle Korrektheit"
            pc <- CGI.p $ Reporter.Wash.embed $ Challenger.partial p i b
            h3 $ CGI.text "totale Korrektheit"
            tc <- CGI.p $ Reporter.Wash.embed $ Challenger.total   p i b
            h3 $ CGI.text $ "Aufgabe gelöst?"
	    bep <- if ( isJust pc && isJust tc )
	       then do
	          CGI.p $ CGI.text $ "Ja."
		  let s = size b
		  CGI.p $ CGI.text $ "Größe der Lösung: " ++ show s
		  -- TODO: ermittle Low/High von DB
		  --		  lift $ unsafe_io 
		  --		       $ bepunkteStudentDB snr anr (Helper.Ok s) $ read hlstr
		  return $ ATB snr anr (Helper.Ok s) $ read hlstr
	       else do 
	           CGI.p $ CGI.text "Nein."
		   return $ ATB snr anr (Helper.No) $ read hlstr 
		  --		  lift $ unsafe_io 
		  --			   $ bepunkteStudentDB snr anr (Helper.No ) $ read hlstr
	    return bep
        Left e -> do 
	   h3 $ CGI.text "Syntaxfehler"
	   CGI.pre $ CGI.text $ render $ errmsg e $ P.input par
	   return $ ATB snr anr (Helper.No) $ read hlstr
		  --	return bep
    ssb ( F1 txtF ) ( evaluator par bep ) "Compute"
    hr CGI.empty       


---------------------------------------------------------------------------

errmsg :: ParseError -> String -> Doc
errmsg e inp = 
    let css = lines inp
	p = errorPos e
	(pre, post) = splitAt (sourceLine p - 3) css
	w = input_width ; c = sourceColumn p - 1
      	top = replicate c '.' ++ replicate (w-c) '^' ; bot = replicate w '-' 
    in  vcat $ map ToDoc.text 
	     $ pre ++ [ top ]
		   ++ [ showErrorMessages 
			      "oder" -- "or" 
			      "unbekannter Parser-Fehler" -- "unknown parse error" 
                              "möglich ist hier:" -- "expecting" 
			      "falsches Zeichen:" -- "unexpected" 
			      "Ende der Eingabe" -- "end of input"
                       (errorMessages e) ]
		   ++ [ bot ]
		   ++ post


    




