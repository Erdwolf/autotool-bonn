module Control.Direktor.CGI 

( main )

where

--  $Id$

import Control.Types
import Gateway.CGI
import Inter.Crypt
import Control.Schule as U
import Control.Student.Type as T
import Control.Student.DB
import qualified Control.Tutor.DB
import qualified Control.Student
import qualified Control.Vorlesung
import Control.Admin.DB
import qualified Control.Schule
import qualified Control.Semester
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


main stud = do
    us <- io $ Control.Direktor.DB.get_directed stud
    when ( not $ null us ) $ do
        open btable
        w <- click_choice_with_default 0 "als Direktor arbeiten?"
                [ ("Nein", False), ("Ja", True ) ]
	close -- btable
        when w $ do
	    open btable
	    u <- click_choice_with_default 0 "für Schule" $ do
                        u <- us
		        return ( toString $ Control.Schule.name u, u )
	    close
            dirigate u 
            mzero
        
dirigate :: Control.Schule.Schule 
	 -> Form IO ()
dirigate u = do
    open btable
    action <- click_choice "Bearbeiten" 
        [ ( "Semesterzeiten", semesterzeiten u )
	, ( "Vorlesungen", vorlesungen u )
	]
    close
    action	  

---------------------------------------------------------------------------

semesterzeiten :: Control.Schule.Schule -> Form IO ()
semesterzeiten u = do
    open btable
    action <- click_choice "Semesterzeiten"
       [ ( "anzeigen", semester_anzeigen )
       , ( "bearbeiten", semester_bearbeiten  )
       , ( "neu anlegen", semester_neu  ) 
       ]
    close
    action u

semester_anzeigen :: Control.Schule.Schule -> Form IO ()
semester_anzeigen u = do
    sems <- io $ Control.Semester.get_at_school $ Control.Schule.unr u
    open btable
    sequence $ do
        sem <- sems
	return $ do
	   open row
           plain $ toString $ Control.Semester.name sem
	   plain $ show sem
	   close -- row
    close -- btable

semester_bearbeiten :: Control.Schule.Schule -> Form IO ()
semester_bearbeiten u = do
    sem <- pick_semester u
    Control.Semester.edit ( Control.Schule.unr u ) 
          ( Control.Semester.enr sem ) ( Just sem )

-- | non-blocking, aktuelles semester als default
pick_semester u = do
    open btable
    sems <- io $ Control.Semester.get_at_school $ Control.Schule.unr u
    sem <- click_choice_with_default 0 "Semester" $ do
        sem <- sortBy ( \ s -> Control.Semester.status s /= Current ) sems
	return ( toString $ Control.Semester.name sem, sem )
    close
    return sem

semester_neu :: Control.Schule.Schule -> Form IO ()
semester_neu u = do
    Control.Semester.edit ( Control.Schule.unr u ) undefined Nothing 

---------------------------------------------------------------------------

vorlesungen :: Control.Schule.Schule -> Form IO ()
vorlesungen u = do
    open btable
    action <- click_choice "Vorlesungen"
       [ ("anzeigen", vorlesungen_anzeigen )
       , ( "bearbeiten", vorlesung_bearbeiten  )
       , ( "neu anlegen", vorlesung_neu  ) 
       ]
    close
    sem <- pick_semester u
    action u sem

vorlesungen_anzeigen ::  Control.Schule.Schule 
		     -> Control.Semester.Semester 
		     -> Form IO ()
vorlesungen_anzeigen u sem = do
    vors <- io $ Control.Vorlesung.get_at_school_sem u sem
    open btable
    sequence $ do
        vor <- vors
	return $ do
	   open row
	   plain $ toString $ Control.Vorlesung.name vor
	   plain $ show vor
	   close
    close

vorlesung_neu :: Control.Schule.Schule 
	     -> Control.Semester.Semester 
	      -> Form IO ()
vorlesung_neu u sem = do
    Control.Vorlesung.edit u Nothing 

vorlesung_bearbeiten :: Control.Schule.Schule 
		     -> Control.Semester.Semester 
		     -> Form IO ()
vorlesung_bearbeiten u sem = do
    vors <- io $ Control.Vorlesung.get_at_school_sem u sem
    open btable
    ( vor , flag ) <- selector_submit_click "wähle" Nothing $ do
        vor <- vors
	return ( toString $ Control.Vorlesung.name vor, vor )
    close
    open btable
    action <- click_choice "Aktion" $ 
        [ ("Parameter", vorlesung_parameter u vor )
	, ("Tutoren", vorlesung_tutoren u vor )
        , ("Vorlesung löschen", Control.Vorlesung.delete u vor )
	]
    close
    action

vorlesung_parameter :: Control.Schule.Schule 
		    -> Control.Vorlesung.Vorlesung 
		    -> Form IO ()
vorlesung_parameter u vor =
    Control.Vorlesung.edit u ( Just vor ) 

vorlesung_tutoren :: Control.Schule.Schule 
		    -> Control.Vorlesung.Vorlesung 
		    -> Form IO ()
vorlesung_tutoren u vor = do
    open btable
    action <- click_choice "Tutoren" $
	  [ ("anzeigen", tutor_anzeigen )
	  , ("hinzufügen", tutor_neu )
	  , ("absetzen", tutor_weg ) 
	  ]
    close -- btable
    action u vor

tutor_anzeigen u vor = do 
    ds <- io $ Control.Tutor.DB.get_tutors vor
    open btable
    sequence $ do
        d <- ds
	return $ do
	    open row
	    plain $ show d
	    close
    close

tutor_neu u vor = do
    studs <- io $ Control.Student.DB.get_unr $ Control.Schule.unr u
    d <- pick_student studs
    up <- submit "ausführen"
    when up $ io $ Control.Tutor.DB.put d vor

tutor_weg u vor = do
    ds <- io $ Control.Tutor.DB.get_tutors vor
    d <- pick_student ds
    up <- submit "ausführen"
    when up $ io $ Control.Tutor.DB.delete d vor

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




