-- | edit the problem configuration
-- and check a sample solution

--  $Id$

module Main where

import Inter.CGI
import Control.Types ( toString )

import qualified Inter.Collector


import Challenger.Partial
import Inter.Types
import Inter.Click

import Inter.Make 
import Inter.Evaluate

import qualified Control.Aufgabe
import qualified Control.Vorlesung


import Autolib.Reporter.Type
import Autolib.ToDoc
import Autolib.Reader


import Random
import Data.Typeable
import Data.Maybe
import Data.List
import Control.Monad


main :: IO ()
main = Inter.CGI.execute "Super.cgi" 
     $ iface Inter.Collector.makers

iface :: [ Make ] -> Form IO ()
iface mks = do

    vors <- io $ Control.Vorlesung.get
    vnr <- selector "vnr" "wähle Vorlesung" 0 $ do
        vor <- vors
        return ( show $ Control.Vorlesung.name vor
	       , Control.Vorlesung.vnr vor 
	       )

    -- FIXME: nicht alle aufgaben aus DB holen, 
    -- sondern nur die mit passender VNr
    aufs <- io $ Control.Aufgabe.get  
    br
    manr  <- selector "anr" "bearbeite Aufgabe" 0 
      $ ( "(neue Aufgabe)", Nothing ) : do
        auf <- aufs
        guard $ Control.Aufgabe.vnr auf == vnr
        return ( show $ Control.Aufgabe.name auf
	       , Just $ Control.Aufgabe.anr auf
	       )
    submit "sauf" "edit"

    pre $ "Vorlesung gewählt : " ++ show vnr
    pre $ "Aufgabe gewählt : " ++ show manr

    mk <- selector "mk" "wähle Aufgabentyp" 0 $ do
        mk <- mks
	return ( show mk, mk )

    case mk of 
        Make doc fun ex -> do
            mconf <- editor "conf" "konfiguriere" ex
            sconf <- submit "sconf" "submit"

            case mconf of
                 Nothing -> return ()
                 Just conf -> do
                      let var :: Var p i b = fun conf
                          p = problem var
                      m0 <- io $ randomRIO (0, 999999 :: Int) 
                      mat <- textfield "mat" $ show m0
                      k <- io $ key var mat -- some key
                      g <- io $ gen var k
                      let ( Just i , com :: Doc ) = export g
                          desc = describe (problem var) i
                          ini = initial (problem var) i
                      h3 "Aufgabenstellung"
                      pre $ show desc
                      mb <- editor "b" "Lösung" ini
                      bconf <- submit "bconf" "submit"
                      case mb of
			   Nothing -> return ()
                           Just b -> do
      	                       let (res, com :: Doc) = export $ evaluate' p i b
                               pre $ show com

