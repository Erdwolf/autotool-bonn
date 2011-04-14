-- | standalone aufgabenconfig,
-- damit jeder mal tutor spielen kann

module Main where

import Gateway.CGI
import Inter.Evaluate
import Inter.Make 
import Inter.Motd
import Inter.Bank
import Inter.Store 
import Inter.Login
import Inter.Logged


import Inter.Tutor
import Inter.Student


import qualified Control.Aufgabe.DB
import qualified Inter.Param as P
import qualified Inter.Statistik

import Gateway.Help

import Autolib.Multilingual ( Language (..), specialize )

import Autolib.Set
import qualified Autolib.Output
import qualified Autolib.Output as O


import qualified Network.CGI

import Control.Types 
    ( toString, fromCGI, Name, Typ , Remark, HiLo (..), Status (..)
    , Oks (..), Nos (..), Time , Wert (..), MNr, SNr, VNr, ANr, UNr
    , TimeStatus (..)
    )

import qualified Control.Types   


import qualified Inter.Collector


import Challenger.Partial
import Inter.Types
import Inter.Common

import Control.Student.CGI
import Control.Vorlesung.DB
import qualified Control.Student.DB
import qualified Control.Punkt
import qualified Control.Stud_Aufg.DB

import qualified Control.Aufgabe as A
import qualified Control.Stud_Aufg as SA
import qualified Control.Student as S
import qualified Control.Vorlesung as V
import qualified Control.Gruppe as G
import qualified Control.Stud_Grp as SG
import qualified Control.Schule as U

import Control.Types ( VNr (..) )

import Autolib.Reporter.Type hiding ( wrap, initial )
import Autolib.ToDoc
import qualified Autolib.Output as O
import Autolib.Reader
import Autolib.Util.Sort
import Autolib.FiniteMap

import qualified Util.Datei as D
import Debug
import qualified Local

import System.Random
import System.Directory
import Data.Typeable
import Data.Maybe
import Data.Tree
import Data.List ( partition )
import Control.Monad
import qualified Text.XHtml

import qualified Autolib.Multilingual as M
import qualified Autolib.Multilingual.Html as H

import Inter.DateTime ( defaults )

my_name = "Trial.cgi"

main :: IO ()
main = Gateway.CGI.execute ( my_name ) $ do
   wrap $ do
       mtopic <- look "topic"
       case mtopic of
           Just topic -> fixed_topic topic
	   Nothing -> do
	       mproblem <- look "problem"
	       case mproblem of
	           Just problem -> fixed_problem problem
		   Nothing -> free_choice

free_choice = do
       selektor
       con <- io $ Inter.Motd.contents
       html con
       hr

btabled :: Monad m => Form m a -> Form m a
btabled = bracketed btable
rowed :: Monad m => Form m a -> Form m a
rowed = bracketed row
mutexed :: ( Typeable a, Monad m ) => Form m () -> Form m a
mutexed action = do begin ; action ; end
bracketed b action = do open b; x <- action ; close ; return x

selektor = do
    hr
    h2 "(Tutor) Aufgabe auswählen und konfigurieren"
    hr
    let tmk = Inter.Collector.tmakers
    action <- btabled $ click_choice "Auswahl..." 
        [ ( "nach Vorlesungen", vor tmk )
	, ( "nach Themen" , aufgaben tmk )
	]
    action dummy

dummy = ( S.Student { }, VNr 42, True )   

vor tmk pack = do
    schulen <- io $ U.get
    schule <- btabled $ click_choice "Hochschule" $ do
        u <- schulen
	return ( toString $ U.name u , u )
    vors <- io $ V.get_at_school $ U.unr schule
    vor <- btabled $ click_choice "Vorlesung" $ do
        v <- vors
	return ( toString $ V.name v , v )
    aufgaben <- io $ A.get ( Just $ V.vnr vor )
    ( conf, auf ) <- mutexed $ btabled $ do
	rowed $ do plain "Aufgabe" ; plain "Typ"
	sequence_ $ do
	    auf <- filter ( \ a -> Early /= A.timeStatus a ) aufgaben
	    return $ rowed $ do
	        plain $ toString $ A.name auf
		plain $ toString $ A.typ  auf
		click ( "solve" , ( False, auf ) )
		click ( "config and solve", ( True, auf ) )
    common_aufgaben tmk pack ( Just auf ) conf   

fixed_problem problem = do
    [ auf ] <- io $ Control.Aufgabe.DB.get_this $ fromCGI problem
    open btable -- ?
    common_aufgaben Inter.Collector.tmakers dummy ( Just auf ) False

fixed_topic topic = do
    let mks =  do Right mk <- flatten Inter.Collector.tmakers ; return mk
    mk : _ <- return $ do 
	 mk @ ( Make _ doc _ _ _ ) <- mks
	 guard $ doc == topic
	 return mk
    open btable -- ?
    common_aufgaben_trailer dummy Nothing True mks mk False

-----------------------------------------------------------------------------

aufgaben tmk pack = do
    common_aufgaben tmk pack Nothing True

common_aufgaben tmk svt @ ( stud, vnr, tutor ) mauf conf = do
    -- plain $ "common_aufgaben.conf: " ++ show conf
    let mks = do Right mk <- flatten tmk ; return mk
    ( mk, type_click ) <- find_mk tmk conf mauf
    -- if the user chose a new type, ignore any predetermined configuration
    let conf' = conf || type_click
        mauf' = if type_click then Nothing else mauf
    common_aufgaben_trailer svt mauf' conf' mks mk type_click

common_aufgaben_trailer ( stud, vnr, tutor ) mauf conf mks mk type_click = do
    -- plain $ "common_aufgaben_trailer.conf: " ++ show conf
    -- plain $ "common_aufgaben_trailer.type_click: " ++ show type_click
    auf' <- case ( mauf, conf ) of
	 ( Just auf, False ) -> return auf
	 _ -> edit_aufgabe_extra mks mk Nothing vnr Nothing type_click
                                 ( \ a -> Early /= A.timeStatus a )
    stud' <- get_stud tutor stud
    hr
    h2 "(Student) Aufgabe lösen"
    hr
    ( minst :: Maybe H.Html, cs, res, com :: Maybe H.Html ) 
        <- solution vnr Nothing stud' mk auf' 
    scores <- scores_link
    hr

    plain "Link zu diesem Aufgabentyp:"
    let target = case mk of 
           Make _ doc _ _ _ -> 
               "Trial.cgi?topic=" ++ Network.CGI.urlEncode doc 
    html $ specialize Autolib.Multilingual.DE  
	 $ ( O.render $ O.Link $ target :: H.Html )
    hr

    case mauf of
      Nothing -> return ()
      Just auf -> do
        plain "Link zu dieser Aufgabeninstanz:"
        let problem = "Trial.cgi?problem=" 
                    ++ Control.Types.toString ( A.anr auf' )
        html $ specialize Autolib.Multilingual.DE  
	     $ ( O.render $ O.Link $ problem :: H.Html )

    footer scores




