-- | re-evaluate all submissions

import qualified Control.Stud_Aufg as SA
import qualified Control.Aufgabe as A
import Control.Types

import Inter.Collector
import Inter.Evaluate
import Inter.Types

import Challenger.Partial

import Autolib.Reporter hiding ( wrap )
import Autolib.ToDoc
import Autolib.Reader

import Control.Exception ( catch )

main :: IO ()
main = wrap "main" $ do
    mapM recompute_for_type $ Inter.Collector.makers
    return ()

wrap msg action = do
    putStrLn $ "start: " ++ msg
    x <- action
    putStrLn $ "end  : " ++ msg
    return x

recompute_for_type mk = 
    wrap ( "for type " ++ show mk ) $ do
        let t = fromCGI $ show mk
        aufs <- A.get_typed t
        mapM ( recompute_for_aufgabe mk ) aufs
        return ()

recompute_for_aufgabe mk @ ( Make p t make v conf ) auf = 
    wrap ("for aufgabe " ++ show auf ) $ do
        eins <- SA.get_anr $ A.anr auf
        mapM ( \ e -> recompute_for_einsendung p t auf ( make conf ) e
                  `Control.Exception.catch` \ any -> return ()
             ) eins
        return ()

recompute_for_einsendung 
    :: ( Reader i, Partial p i b , V p i b  )
    => p -> String -> A.Aufgabe -> Var p i b -> SA.Stud_Aufg -> IO ()
recompute_for_einsendung   p tag auf v eins = 
    wrap ("for einsendung " ++ show eins ) $ do
        instant  <- if True 
                    then parse_from_file   ( SA.instant eins ) 
                    else return $ get_i_type v
        input   <- read_from_file   ( SA.input   eins )
        let ( res, doc :: Doc ) = export $ evaluate p instant input
        let old_result = SA.result  eins
        when ( res /= old_result ) $ do
              putStrLn $ show $ vcat
                   [ text "tag:" <+> toDoc tag
                   , text "aufgabe:" <+> toDoc auf
                   , text "einsendung:" <+> toDoc eins
                   , text "instant:" <+> toDoc instant
                   , text "input:" <+> text input
                   , text "comment:" <+> doc
                   , text "computed result:" <+> toDoc res
                   , text "stored result:" <+> toDoc old_result
                   ]

parse_from_file :: ( ToDoc a, Reader a ) => Maybe File -> IO a
parse_from_file ( Just fname ) = do
    input <- readFile $ toString fname
    parse_from_string input

parse_from_string :: ( ToDoc a, Reader a ) => String -> IO a
parse_from_string input = do
    case result $ parse_or_complain input of
        Just it -> return it
        Nothing -> error "no parse"

read_from_file :: Maybe File -> IO String
read_from_file ( Just fname ) = do
    this <- readFile $ toString fname
    return this

