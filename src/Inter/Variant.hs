module Inter.Variant where

-- $Id$

import Inter.Types
import qualified Inter.Param as P

import Reporter
import Reporter.Washer
import Challenger
import Step


-- wash
import CGI hiding ( span, map, div, head, name )
import qualified CGI
-- control
import HTMLshortcuts 


interface :: [ Variant ] -> IO ()
interface vs = CGI.run [] $ login ( P.empty { P.variants = vs } ) F0

login p F0 = standardQuery "Login" $ preface p

preface p = do
    matF <- promptedInput "Matrikelnr:" 
	    $ (fieldSIZE 30) ## ( fieldVALUE $ P.matrikel p )
    pwdF <- promptedPassword "Passwort:" 
	    $ (fieldSIZE 30) ## ( fieldVALUE $ P.passwort p )
    proF <- promptedInput "Aufgabentyp:" 
	    $ (fieldSIZE 30) ## ( fieldVALUE $ P.problem p )
    varF <- promptedInput "Variante:" 
	    $ (fieldSIZE 30) ## ( fieldVALUE $ P.variant p )
    let args =  F4 matF pwdF proF varF
    smallSubButton args (computer p) "Login"

computer p0 (F4 matF pwdF proF varF) = do
    let p = p0 { P.matrikel = value matF
	       , P.passwort = value pwdF
	       , P.problem  = value proF
	       , P.variant  = value varF
	       }
    case P.variants p !! 0 of
         Variant v -> do

            k <- unsafe_io $ key v ( P.matrikel p )
	    let fname = k ++ ".html"  -- TODO
            Just i <- unsafe_io $ washer fname $ gen v ( P.matrikel p )
	    let b0 = initial ( Inter.Types.problem v ) i

            -- muß hier stehen, da sonst die typen escapen
            let evaluator p (F1 txtF) = do
                  return ()

            standardQuery "Computer" $ do
                -- preface p
                iframe $ attr "src" fname
		txtF <- makeTextarea ( show b0 ) CGI.empty
                smallSubButton (F1 txtF) (evaluator p) "Compute"



    




