{-# OPTIONS -fglasgow-exts #-}

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

import qualified Inter.Param as P
import qualified Inter.Statistik

import Gateway.Help

import Autolib.Set
import qualified Autolib.Output

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

import Inter.DateTime ( defaults )

my_name = "Trial.cgi"

main :: IO ()
main = Gateway.CGI.execute ( my_name ++ "#hotspot" ) $ do
   let stud = S.Student { }
   wrap $ selektor stud

btabled :: Monad m => Form m a -> Form m a
btabled = bracketed btable
rowed :: Monad m => Form m a -> Form m a
rowed = bracketed row
mutexed :: ( Typeable a, Monad m ) => Form m () -> Form m a
mutexed action = do begin ; action ; end
bracketed b action = do open b; x <- action ; close ; return x

selektor stud = do
    h3 "Aufgaben"
    let tmk = Inter.Collector.tmakers
    action <- btabled $ click_choice "Auswahl..." 
        [ ( "nach Vorlesungen", vor tmk )
	, ( "nach Themen"
	  , aufgaben tmk 
	  )
	]
    action ( stud, VNr 42, True )   

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
	    auf <- aufgaben
	    return $ rowed $ do
	        plain $ toString $ A.name auf
		plain $ toString $ A.typ  auf
		click ( "solve" , ( False, auf ) )
		click ( "config and solve", ( True, auf ) )
    common_aufgaben tmk pack ( Just auf ) conf   

aufgaben tmk pack = do
    common_aufgaben tmk pack Nothing True

common_aufgaben tmk ( stud, vnr, tutor ) mauf conf = do
    let mks = do Right mk <- flatten tmk ; return mk
    ( mk, type_click ) <- find_mk tmk True mauf
    auf' <- case ( mauf, conf ) of
	 ( Just auf, False ) -> return auf
	 _ -> edit_aufgabe mks mk Nothing vnr Nothing type_click
    stud' <- get_stud tutor stud
    solution vnr Nothing stud' mk auf' 
    scores <- scores_link
    footer scores




