{-# LANGUAGE DeriveDataTypeable #-}
module Control.Admin.CGI 

( main )

where

--  $Id$

import Control.Types
import Gateway.CGI
import Inter.Crypt
import Control.Schule as U
import Control.Student.Type as T
import Control.Student.DB
import qualified Control.Student
import Control.Admin.DB
import qualified Control.Schule
import qualified Control.Schule.DB
import qualified Control.Schule.CGI
import qualified Control.Direktor.DB

import Control.Monad
import Data.List ( partition, isSuffixOf )
import Data.Char ( isAlphaNum )
import Data.Maybe ( isNothing , isJust , fromJust )
import Data.Typeable

import Autolib.Util.Zufall
import Autolib.Util.Sort

import qualified Debug
import qualified Local

main :: Control.Student.Student -> Form IO ()
main s = do
    m <- io $ is_minister s
    when m $ do
        open btable
        w <- click_choice_with_default 0 "als Admin arbeiten?"
                [ ("Nein", False), ("Ja", True ) ]
        close -- btable
	when w $ do administrate ; mzero

data Action = Edit |  Direct
    deriving ( Eq, Show, Typeable )
        
administrate :: Form IO ()
administrate = do
    us <- io $ Control.Schule.DB.get
    open btable
    mu <- click_choice "Hochschule" $ 
            ( "neu anlegen", Nothing ) : do
	        u <- us
		return ( toString ( Control.Schule.name u ) 
		       , Just u
		       )
    close -- btable
    case mu of
        Nothing -> Control.Schule.CGI.edit undefined Nothing
        
	Just u -> do
            open btable
            act <- click_choice "Aktion"
		[ ("bearbeiten", Edit )
		, ("Direktoren", Direct )
		]
	    close -- btable      
            case act of
	        Edit -> Control.Schule.CGI.edit 
		    ( Control.Schule.unr u ) ( Just u )
	        Direct -> direct u

direct :: Control.Schule.Schule 
       -> Form IO ()
direct u = do
    ds <- io $ Control.Direktor.DB.get_directors u
    open btable
    onoff <- click_choice "Direktor" $
	  [ ( "hinzufügen", True ), ("absetzen", False) ]
    close -- btable
    case onoff of
        True -> do
            studs <- io $ Control.Student.DB.get_unr $ Control.Schule.unr u
            d <- pick_student studs
            up <- submit "ausführen"
            when up $ io $ Control.Direktor.DB.put d u
        False -> do
            d <- pick_student ds
            up <- submit "ausführen"
            when up $ io $ Control.Direktor.DB.delete d u

pick_student :: [ Control.Student.Student ] 
	     -> Form IO Control.Student.Student
pick_student studs = do
    open btable
    ( s, flag ) <- selector_submit_click "wähle:" Nothing $ do
        stud <- sortBy Control.Student.name studs
        let s = unwords [ toString $ Control.Student.mnr stud
                        , toString $ Control.Student.vorname stud
                        , toString $ Control.Student.name stud
                        ]
        return (s, stud)
    close 
    return s



