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

-- damit die versteckte history nicht zu lang wird:
-- neues konzept, student muss immer zurueck 
-- button des browsers nehmen

input_width :: Int
-- for textarea
input_width = 70 
-- height wird automatisch berechnet

interface :: [ Variant ] -> IO ()
interface vs = do
    user <- Posix.getEffectiveUserName
    Posix.setEnvVar "HOME" user
    CGI.run [] $ login ( P.empty { P.variants = vs } ) 

texti :: String 
      ->  WithHTML CGI ()
      -> WithHTML CGI (InputField String INVALID) 
texti txt attrs = tr $ do 
      td $ CGI.text txt 
      td $ textInputField attrs

login :: P.Type -> CGI ()
login par = 
    standardQuery "Login zum Autotool-Webinterface" $ -- preface par
    table $ do
    	matF <- tr $ texti "Matrikelnr:" 
        	$ (fieldSIZE 30) ## ( fieldVALUE $ P.matrikel par )
	pwdF <- tr $ promptedPassword "Passwort:" 
        	$ (fieldSIZE 30) ## ( fieldVALUE $ P.passwort par )
	tr $ ssb (F2 matF pwdF) (chooseTypPage par) "Login"

--
-- login ueberpruefen, auswahl der aufgabe
--
chooseTypPage par0 (F2 matF pwdF) = 
    let par = par0 { P.matrikel = value matF 
		   , P.passwort = value pwdF
		   , P.input    = ""
		   }
    in do  
       mbsnr <- io $ loginDB (P.matrikel par) (show (P.passwort par))
       case mbsnr of 
         Nothing -> standardQuery "Login" $ CGI.text
	   "Fehlgeschlagen!" -- ssb F0 (login par) "login wiederholen"
	 Just snr -> 
	   do 
	   aufgs <- io $ mglAufgabenDB snr
	   standardQuery "Aufgabe Auswaehlen" $ 
	     do
	     let shownamesub x@(_,n,s,_,_) = n ++ " - " ++ s 
	     CGI.h1 $ CGI.text $ "Bitte zuloesende Aufgabe auswaehlen"
             aufg <- selectSingle shownamesub Nothing aufgs CGI.empty
	     submit (F1 aufg) (prepareInputPage $ par { P.snr = snr }) (CGI.fieldVALUE "gewaehlt.")
	     return ()
	     --			computer (par { P.snr = snr }) ATBEmpty





----------------------------------------------------------------------

-- -- rechnet alles aus
-- computer par bep = 
--     let
-- 	vvs :: [ Variant ]
--         vvs = do
-- 	   vv <- P.variants par
-- 	   case vv of 
-- 	       Variant v -> do
-- 	           guard $ P.problem par  == show ( Inter.Types.problem v )
-- 	           guard $ P.variant par  ==        Inter.Types.variant v
-- 	           return vv
--     in case vvs of
--          [ vv ] -> handler par vv bep
--          _      -> error_handler par $ preface par



-- eingabe der lsg vorbereiten,
-- a) instanz generieren
prepareInputPage par0 (F1 aufg0) = 
	-- selektierte Aufg, holen, speichern
	let { (_,prob,vari,_,_) = value $ aufg0
		; par 			  = par0 { P.problem = prob, P.variant = vari }
		  -- passende Variante bestimmen
		; vvs :: [ Variant ]
      	; vvs = [ vv |  vv@(Variant v) <- P.variants par
				, P.problem par  == show ( Inter.Types.problem v )
	           	, P.variant par  ==        Inter.Types.variant v
				]
		}
	-- existiert die Variante ??
	in case vvs of
         [ vv ] -> do 
  		 			-- generier einer Instanz (kann abh. von matrikel / zeit sein)
					--  k    <- io $ key v ( P.matrikel par0 )
  					-- inst <- io $ washer $ gen v ( P.matrikel par0 )
  				   let i = gen_i vv ( P.matrikel par0 )
--				   inputLsg par i vv
				   standardQuery "TEST" CGI.empty
				   return ()
         _      -> error_handler par $ return () -- CGI.empty

-- Loesung eingeben:

-- inputLsgPage par i v = 
-- 	standardQuery "Eingabe der Loesung" $ 
-- 		do
--    	    -- ausgabe der aufgaben - instanz
--     	hr CGI.empty
-- 	    h2 $ CGI.text $ "Die Aufgaben-Instanz:"
--     	CGI.pre $ CGI.text $ show i
-- 	    let p 	= Inter.Types.problem v
-- 	    	par = if null $ P.input par0
-- 		      	  then par0 { P.input = show $ Challenger.initial p i }
-- 	    	  	  else par0
-- 		-- eingabe des beweises (lsg)
-- 	    hr CGI.empty
-- 	    h2 $ CGI.text "Lösungs-Versuch"    
-- 	    txtF <- table $ tr $ 
-- 			do
-- 	        let height = length $ filter ( == '\n' ) $ P.input par
-- 			txtF <- td $ makeTextarea ( P.input par ) 
-- 			   $  attr "rows" ( show $ height + 2 )
-- 			   ## attr "cols" ( show input_width )
-- 			return txtF
	--	return ()
    --        td $ ssb ( F1 txtF ) ( evaluator par ) "Compute"

--    submit (F1 lsg) computer (fieldVALUE="compute")  

-- Ergebnis berechnen

-- wenn kontrolleur gefunden,
-- dann instanz und nullte lösung berechnen
-- computer par0  (Variant v ) =  
--   standardQuery "Computer" $ do
--     let { [(anr,hlstr)] = ahs 
-- 		; snr = P.snr par0 
-- 		} 
--     hr CGI.empty
--     preface par0

--     -- Zeige Ergebnisse dieser Aufgabe
--     ergtabl @ (header , ergs) <- lift $ unsafe_io $ studAufgDB (P.matrikel par0)
--     let erg = [ e | e@[_,S  t,S s,_,_] <- ergs
-- 	      , (P.problem par0) == t
-- 	      , (P.variant par0) == s
-- 	      ]
 	
    
    
--     -- TODO: die folgenden Zeilen mit Cache !

--     hr CGI.empty
--     h2 $ CGI.text $ "Die Aufgaben-Instanz: Nr." ++ anr

--     CGI.pre $ CGI.text $ show i

--     let p = Inter.Types.problem v

--     let par = if null $ P.input par0
-- 	      then par0 { P.input = show $ Challenger.initial p i }
-- 	      else par0
--     if null erg then CGI.empty else showAsTable (header,erg)
--     CGI.text $ show bep
--     hr CGI.empty
--     h2 $ CGI.text "neuer Lösungs-Versuch"    
--     txtF <- table $ tr $ do
--         let height = length $ filter ( == '\n' ) $ P.input par
-- 	txtF <- td $ makeTextarea ( P.input par ) 
-- 		   $  attr "rows" ( show $ height + 2 )
-- 		   ## attr "cols" ( show input_width )
-- 	return txtF
--     --        td $ ssb ( F1 txtF ) ( evaluator par ) "Compute"
--     hr CGI.empty
--     h2 $ CGI.text "voriger Lösungs-Versuch"

--     bep <- case parse reader "input" $ P.input par of
--         Right b -> do
-- 	    CGI.pre $ CGI.text $ P.input par
--             h3 $ CGI.text "partielle Korrektheit"
--             pc <- CGI.p $ Reporter.Wash.embed $ Challenger.partial p i b
--             h3 $ CGI.text "totale Korrektheit"
--             tc <- CGI.p $ Reporter.Wash.embed $ Challenger.total   p i b
--             h3 $ CGI.text $ "Aufgabe gelöst?"
-- 	    bep <- if ( isJust pc && isJust tc )
-- 	       then do
-- 	          CGI.p $ CGI.text $ "Ja."
-- 		  let s = size b
-- 		  CGI.p $ CGI.text $ "Größe der Lösung: " ++ show s
-- 		  -- TODO: ermittle Low/High von DB
-- 		  --		  lift $ unsafe_io 
-- 		  --		       $ bepunkteStudentDB snr anr (Helper.Ok s) $ read hlstr
-- 		  return $ ATB snr anr (Helper.Ok s) $ read hlstr
-- 	       else do 
-- 	           CGI.p $ CGI.text "Nein."
-- 		   return $ ATB snr anr (Helper.No) $ read hlstr 
-- 		  --		  lift $ unsafe_io 
-- 		  --			   $ bepunkteStudentDB snr anr (Helper.No ) $ read hlstr
-- 	    return bep
--         Left e -> do 
-- 	   h3 $ CGI.text "Syntaxfehler"
-- 	   CGI.pre $ CGI.text $ render $ errmsg e $ P.input par
-- 	   return $ ATB snr anr (Helper.No) $ read hlstr
-- 		  --	return bep
--     ssb ( F1 txtF ) ( evaluator par bep ) "Compute"
--     hr CGI.empty       


---------------------------------------------------------------------------

-- errmsg :: ParseError -> String -> Doc
-- errmsg e inp = 
--     let css = lines inp
-- 	p = errorPos e
-- 	(pre, post) = splitAt (sourceLine p - 3) css
-- 	w = input_width ; c = sourceColumn p - 1
--       	top = replicate c '.' ++ replicate (w-c) '^' ; bot = replicate w '-' 
--     in  vcat $ map ToDoc.text 
-- 	     $ pre ++ [ top ]
-- 		   ++ [ showErrorMessages 
-- 			      "oder" -- "or" 
-- 			      "unbekannter Parser-Fehler" -- "unknown parse error" 
--                               "möglich ist hier:" -- "expecting" 
-- 			      "falsches Zeichen:" -- "unexpected" 
-- 			      "Ende der Eingabe" -- "end of input"
--                        (errorMessages e) ]
-- 		   ++ [ bot ]
-- 		   ++ post


    




