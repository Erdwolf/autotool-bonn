{-# LANGUAGE ScopedTypeVariables #-}
module Inter.Tutor where


import Inter.Types
import Inter.Bank

import Gateway.CGI
import qualified Inter.Param as P
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import Challenger.Partial
import Control.Types 
    ( toString, fromCGI, Name, Typ , Remark, HiLo (..), Status (..)
    , Oks (..), Nos (..), Time , Wert (..), MNr, SNr, VNr, ANr, UNr
    , TimeStatus (..)
    )
import Control.Time
import Control.Time.CGI
import qualified Control.Time.CGI as Control.Time

import Autolib.Reporter.IO.Type
import Autolib.ToDoc
import Autolib.Reader

import qualified Text.XHtml
import System.Random
import Data.Maybe
import Data.Tree ( flatten )


import qualified Control.Exception as CE

-- | ändere aufgaben-konfiguration (nur für tutor)
edit_aufgabe mks mk mauf vnr manr type_click = 
    edit_aufgabe_extra mks mk mauf vnr manr type_click ( \ a -> True )

edit_aufgabe_extra mks mk mauf vnr manr type_click prop = case mk of 
        Make p doc ( fun :: conf -> Var p i b ) verify ex -> do

            let t = fromCGI $ show mk

            candidates <- io $ A.get_typed t
               `CE.catch` \ (CE.SomeException any) -> return []
            let others = filter prop candidates

            ( name :: Name ) <- fmap fromCGI 
		     $ defaulted_textfield "Name" 
		     $ case mauf of Nothing -> foldl1 (++) 
					       [ toString t , "-"
					       , show $ succ $ length others 
					       ]
                                    Just auf -> toString $ A.name auf
	    ( remark :: Remark ) <- fmap fromCGI 
		    $ defaulted_textarea "Remark" 
		    $ case mauf of Nothing -> "noch keine Hinweise"
	                           Just auf -> toString $ A.remark auf
            open row
            plain "Highscore"
	    ( mhilo :: Maybe HiLo ) <- selector' 
                ( case mauf of Nothing -> "XX"
		               Just auf -> show $ A.highscore auf )
                   $ do
		     ( x :: HiLo ) <- [ minBound .. maxBound ]
                     return ( show x, x )
            close -- row
            open row
            plain "Status"
	    ( mstatus :: Maybe Status ) <- selector' 
                ( case mauf of Nothing -> "XX"
		               Just auf -> show $ A.status auf )
                   $ do
		     ( x :: Status ) <- [ minBound .. maxBound ]
                     return ( show x, x )
            close -- row

            (dflt_von,dflt_bis) <- io Control.Time.defaults

            ( von :: Time ) <- Control.Time.edit "von" $ Just
		    $ case mauf of Nothing -> dflt_von
				   Just auf -> A.von auf
            ( bis ::Time ) <- Control.Time.edit "bis" $ Just 
		    $ case mauf of Nothing -> dflt_bis
				   Just auf -> A.bis auf

            moth <- 
                click_choice_with_default 0 "importiere Konfiguration" 
                     $  ("(default)", mauf) : do
                           oth <- others
                           return ( toString $ A.name oth , Just oth )
            let ( mproto, type_changed ) = case moth of
                   Just oth -> ( moth, False )
                   Nothing  -> ( mauf, type_click )

            -- nimm default-config, falls type change 
            -- FIXME: ist das sinnvoll bei import?
            conf <- editor_submit "Konfiguration" 
		    $ case mproto of 
			  Just auf | not type_changed   -> 
			       case parse ( parsec_wrapper 0 ) "input"
				          ( toString $ A.config auf ) of
				        Right ( x, rest ) -> x
					Left err -> ex
			  _ -> ex

	    close -- table

	    -- check configuration

	    br
	    
	    Just _ <- embed $ do
		 inform $ text "verifiziere die Konfiguration:"
	         verifyIO p conf
		 inform $ text "OK"
            br
            return $ A.Aufgabe 
		               { A.anr = error "Super.anr" -- intentionally
			       , A.vnr = vnr
			       , A.name = name
			       , A.typ = fromCGI $ show mk
			       , A.config = fromCGI $ show conf
			       , A.remark = remark
			       , A.highscore = fromMaybe Keine mhilo
			       , A.status = fromMaybe Demo mstatus
			       , A.von = von
			       , A.bis = bis
			       , A.timeStatus = Control.Types.Early -- ist egal
			       }

-- | matrikelnummer zum aufgabenlösen:
-- tutor bekommt eine gewürfelt (und kann neu würfeln)
-- student bekommt genau seine eigene
get_stud tutor stud = 
    if tutor 
       then do
         hr
	 m0 <- io $ randomRIO (0, 999999 :: Int) 
	 -- neu würfeln nur bei änderungen oberhalb von hier
	 plain "eine gewürfelte Matrikelnummer:"
	 mat <- with ( show m0 ) $ textfield ( show m0 )
         -- falls tutor, dann geht es hier nur um die matrikelnr
	 return $ stud { S.mnr = fromCGI mat
		       , S.snr = error "gibt es nicht"
		       }
       else do
	 return stud


-- | bestimme aufgaben-typ (maker)
-- für tutor: wählbar
-- für student: fixiert (ohne dialog)
find_mk tmk tutor mauf = do
    let pre_mk = fmap (toString . A.typ) mauf
    if tutor 
            then do
		 hr
		 h3 "Parameter dieser Aufgabe:"
		 open btable -- will be closed in edit_aufgabe (tutor branch)
		 -- selector_submit_click "Typ" pre_mk opts
                 it <- tree_choice pre_mk $ fmap ( \ n -> case n of
                                 Right mk -> Right ( show mk, mk )
                                 Left heading -> Left $ heading ++ " .."
                                    ) tmk
                 return ( it, True ) -- FIXME
            else do
		 Just pre <- return $ pre_mk
                 let mks = do 
                        Right mk <- flatten tmk
                        return ( show mk, mk )
		 Just it  <- return $ lookup pre mks
		 return ( it, False )


