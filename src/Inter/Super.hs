-- | edit the problem configuration
-- and check a sample solution

--  $Id$

module Main where

import Inter.CGI
import Control.Types ( toString, fromCGI, Name, Remark, HiLo (..), Time )


import qualified Inter.Collector


import Challenger.Partial
import Inter.Types

import Control.Student.CGI
import Control.Tutor

import Inter.Make 
import Inter.Evaluate

import qualified Control.Aufgabe as A
import qualified Control.Student.Type as S
import qualified Control.Vorlesung as V


import Autolib.Reporter.Type
import Autolib.ToDoc
import Autolib.Reader

import Random
import Data.Typeable
import Data.Maybe
import Data.List
import Control.Monad

import Text.Html ( Html )

main :: IO ()
main = Inter.CGI.execute "Super.cgi" 
     $ iface Inter.Collector.makers

iface :: [ Make ] -> Form IO ()
iface mks = do

    stud <- Control.Student.CGI.login
    let snr = S.snr stud

    open btable
 
    tvors <- io $ V.get_tutored snr
    -- unterschiede: tutor darf "alles",
    -- student darf keine aufgaben ändern und nur aktuelle aufgaben sehen
    let tutor = not $ null tvors
    vors <- if tutor 
            then return tvors
	    else io $ V.get_attended snr

    vnr <- selector_submit "vnr" "Vorlesung" 0 $ do
        vor <- vors
        return ( show $ V.name vor , V.vnr vor )

    aufs <- io $ A.get vnr ( not tutor ) -- student darf nur aktuelle
    let opts = do
                     auf <- aufs
                     return ( show $ A.name auf , Just $ auf )
    ( mauf, del ) <- 
        if tutor
	   then selector_edit_delete "anr" "Aufgabe" 0 
                   $ ( "(neue Aufgabe)", Nothing ) : opts
           else do 
		( mauf, _ ) <- selector_submit_click "anr" "Aufgabe" Nothing $ opts
                return ( mauf, False )
    let manr = fmap A.anr mauf
    close -- table
    
    when del $ do
        Just anr <- return manr
        io $ A.delete anr
        plain $ unwords [ "Aufgabe", show anr, "gelöscht." ]
	mzero

    hr
    ( mk, type_click ) <- find_mk mks tutor mauf
    auf' <- edit_aufgabe mk mauf vnr manr type_click


    hr ---------------------------------------------------------
    m0 <- io $ randomRIO (0, 999999 :: Int) 
    -- neu würfeln nur bei änderungen oberhalb von hier
    plain "eine gewürfelte Matrikelnummer:"
    mat <- with ( show m0 ) $ textfield "mat" ( show m0 )

    solution mat mk auf' 


find_mk mks tutor mauf = do
    h3 "Parameter dieser Aufgabe:"
    open btable

    let pre_mk = fmap (toString . A.typ) mauf
    let handler = 
            if tutor 
            then selector_submit_click "typ" "Typ"
            else \ pre_mk opts -> return 
		    ( fromMaybe (error "oof") $ do
                          pre <- pre_mk ; lookup pre opts
                    , False
		    ) 
    handler pre_mk $ do
        mk <- mks
	return ( show mk, mk )

edit_aufgabe mk mauf vnr manr type_click = do
    case mk of 
        Make doc ( fun :: conf -> Var p i b ) ex -> do
            ( name :: Name ) <- fmap fromCGI 
		     $ defaulted_textfield "Name" 
		     $ case mauf of Nothing -> "noch kein Name"
                                    Just auf -> toString $ A.name auf
	    ( remark :: Remark ) <- fmap fromCGI 
		    $ defaulted_textarea "Remark" 
		    $ case mauf of Nothing -> "noch keine Hinweise"
	                           Just auf -> toString $ A.remark auf
            open row
            plain "Highscore"
	    ( mhilo :: Maybe HiLo ) <- selector' "Highscore"  
                ( case mauf of Nothing -> "XX"
		               Just auf -> show $ A.highscore auf )
                   $ do
		     ( x :: HiLo ) <- [ minBound .. maxBound ]
                     return ( show x, x )
            close -- row
            -- FIXME: get now() from DB
            ( von :: Time ) <- fmap fromCGI 
		    $ defaulted_textfield "von" 
		    $ case mauf of Nothing -> "2004-10-15 11:16:08"
				   Just auf -> toString $ A.von auf
            ( bis ::Time ) <- fmap fromCGI
		    $ defaulted_textfield "bis" 
		    $ case mauf of Nothing -> "2004-10-15 11:16:08"
				   Just auf -> toString $ A.bis auf
	    close -- table

            -- nimm default-config, falls type change
            conf <- editor_submit "conf" "Konfiguration" 
		    $ case mauf of 
			  Just auf | not type_click  -> 
				 read $ toString $ A.config auf
			  _ -> ex :: conf
				   
            br
	    up <- submit "update" "update data base"
            let auf' = A.Aufgabe 
		               { A.anr = error "Super.anr" -- intentionally
			       , A.vnr = vnr
			       , A.name = name
			       , A.typ = fromCGI $ show mk
			       , A.config = fromCGI $ show conf
			       , A.remark = remark
			       , A.highscore = fromMaybe Keine mhilo
			       , A.von = von
			       , A.bis = bis
			       }
            when up $ io $ A.put manr auf'
            return auf'




solution mat ( Make doc ( fun :: conf -> Var p i b ) ex ) auf = do
            let conf = read $ toString $ A.config auf
                var = fun  conf
                p = problem var
            k <- io $ key var mat 
            g <- io $ gen var k
            let ( Just i  , com :: Doc ) = export g
                desc = describe (problem var) i
                ini  = initial  (problem var) i
            h3 "Aufgabenstellung"
            pre $ show desc
 
            hr ---------------------------------------------------------

            b <- editor_submit "b" "Lösung" ini
     	    let (res, com :: Html) = export $ evaluate' p i b
            -- pre $ show com
            html com
	    -- TODO: bewertung in DB (für Stud-Variante)

--------------------------------------------------------------------------

-- TODO: move to separate module (chop Control.CGI into pieces as well)

defaulted_selector tag def opts = do
    open row
    plain tag
    ms <- selector tag def opts
    close
    return $ fromMaybe ( snd $ opts !! def ) ms

defaulted_textfield tag def = do
    open row
    plain tag
    ms <- textfield tag def
    close -- row
    return $ fromMaybe def ms

defaulted_textarea tag def = do
    open row
    plain tag
    ms <- textarea tag def
    close -- row
    return $ fromMaybe def ms

selector_submit tag title def opts = do
    open row
    plain title
    mopt <- selector ("L" ++ tag) def opts
    sopt <- submit   ("S" ++ tag) "submit"
    close -- row
    case mopt of
	 Nothing -> mzero
	 Just opt -> do
	      when sopt blank
	      return opt

selector_edit_delete tag title def opts = do
    open row
    plain title
    mopt <- selector ("L" ++ tag) def opts
    sopt <- submit   ("S" ++ tag) "edit"
    dopt <- submit   ("D" ++ tag) "delete"
    close -- row
    case mopt of
	 Nothing -> mzero
	 Just opt -> do
	      when sopt blank
	      return ( opt, dopt )


-- | if default is Nothing, then stop here, else continue with default
selector_submit_click :: Monad m
	         => Tag 
		 -> String 
		 -> Maybe String -- ^ possible default
		 -> [(String, a) ] 
		 -> Form m (a, Bool)
selector_submit_click tag title def opts = do
    open row
    plain title
    mopt <- selector' ("L" ++ tag) (fromMaybe "XX" def) opts
    sopt <- submit   ("S" ++ tag) "submit"
    close
    case mopt of
	 Nothing -> case def of
	      Nothing -> mzero
	      Just d -> case lookup d opts of
		   Nothing -> mzero
                   Just opt -> return ( opt, False )
	 Just opt -> do
	      when sopt blank
	      return ( opt, sopt )

selector_submit' tag title def opts = do
    ( opt, sopt ) <- selector_submit_click tag title def opts
    return opt

editor_submit :: ( ToDoc a, Reader a, Monad m )
	      => Tag -> String -> a 
	      -> Form m a
editor_submit tag title ex = do
    open row
    plain title
    mconf <- editor ("E" ++ tag) ex
    sconf <- submit ("S" ++ tag) "submit"
    close -- row
    case mconf of
	 Nothing -> do
	     -- mzero
	     return ex
	 Just conf -> do
             when sconf blank
	     return conf
