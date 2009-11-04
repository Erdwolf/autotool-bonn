module Inter.Recommon where

import qualified Control.Stud_Aufg as SA
import qualified Control.Aufgabe as A
import qualified Control.Student as S
import Control.Types

import Inter.Collector
import Inter.Evaluate
import Inter.Common
import Inter.Types
import qualified Inter.Param as P
import qualified Inter.Bank

import qualified Util.Datei
import Challenger.Partial

import Autolib.Reporter hiding ( wrap )
import Autolib.ToDoc
import Autolib.Reader

import Control.Exception ( catch )
import System.IO
import qualified System.Time
import qualified System.Directory

verbose :: Bool
verbose = False -- True

recompute_for_einsendung 
    :: Make -> A.Aufgabe -> SA.Stud_Aufg -> IO ()
recompute_for_einsendung  mk auf eins = 
    {- wrap ("for einsendung " ++ show eins ) $ -} do
        studs <- S.get_snr $ SA.snr eins
        mapM ( \ s -> recompute_for_student mk auf eins s 
		  `Control.Exception.catch` \ any -> do
	              hPutStrLn stderr $ "err: " ++ show any
	     ) studs
        return ()

recompute_for_student ( Make p tag fun verify conf ) auf eins stud = do
        ( p, instant, icom ) <- 
            make_instant_common (A.vnr auf) (Just $ A.anr auf) stud 
                   ( fun $ read $ toString $ A.config auf ) 
        input   <- read_from_file $ SA.input eins
        let ( res, doc :: Doc ) = export $ evaluate p instant input
        let old_result = SA.result  eins
        if ( compatible old_result res )
           then do
              hPutStr stderr "."
           else do
              hPutStrLn stderr $ show $ vcat
                   [ text "tag:" <+> toDoc tag
                   , text "aufgabe:" <+> toDoc auf
                   , text "einsendung:" <+> toDoc eins
                   , text "instant:" <+> toDoc instant
                   , text "input:" <+> text input
                   , text "comment:" <+> doc
                   , text "computed result:" <+> toDoc res
                   , text "stored result:" <+> toDoc old_result
                   ]
              hFlush stderr
        let Just inf = SA.input   eins
        clock <- System.Directory.getModificationTime $ toString inf
        cal <- System.Time.toCalendarTime clock    
        let time = System.Time.calendarTimeToString cal
        let param = P.Param { P.mmatrikel = Just $ S.mnr stud 
                            , P.vnr = A.vnr auf
                            , P.anr = A.anr auf
                            }
        case res of
            Just res -> do
                putStr $ Inter.Bank.logline time "771" param res
            Nothing -> return ()


compatible ( Just Pending ) _ = True
compatible ( Just No ) Nothing = True
compatible x y = x == y

{-

parse_from_file :: ( ToDoc a, Reader a ) => Maybe File -> IO a
parse_from_file ( Just fname ) = do
    input <- readFile $ toString fname
    parse_from_string input

parse_from_string :: ( ToDoc a, Reader a ) => String -> IO a
parse_from_string input = do
    case result $ parse_or_complain input of
        Just it -> return it
        Nothing -> error "no parse"

-}

read_from_file :: Maybe File -> IO String
read_from_file ( Just fname ) = do
    this <- readFile $ toString fname
    return this

