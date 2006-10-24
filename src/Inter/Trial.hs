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
   wrap $ aufgaben Inter.Collector.tmakers ( undefined, VNr 42, True )

aufgaben tmk ( stud, vnr, tutor ) = do
    h3 "Aufgaben"

    let mks = do Right mk <- flatten tmk ; return mk

    ( mk, type_click ) <- find_mk tmk True Nothing
    auf' <- edit_aufgabe mks mk Nothing vnr Nothing type_click
    stud' <- get_stud tutor stud
    solution vnr Nothing stud' mk auf' 
    scores <- scores_link
    footer scores




