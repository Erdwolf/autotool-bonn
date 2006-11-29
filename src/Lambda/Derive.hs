{-# OPTIONS -fglasgow-exts #-}

module Lambda.Derive where

import Lambda.Type
import Lambda.Tree ( peng )
import Lambda.Derive.Instance as I
import Lambda.Step

import Challenger.Partial
import Inter.Types
import Autolib.ToDoc
import Autolib.Reporter
import Data.Typeable

data Derive = Derive 
    deriving ( Read, Show, Typeable )

instance Measure Derive I.Type [ Int ] where
    measure p inst xs = fromIntegral $ length xs

instance Partial Derive I.Type [ Int ] where
    report p inst = do
        let step_info = case steps inst of
               Nothing -> empty
               Just s  -> text "der Länge" <+> toDoc s
        inform $ vcat
            [ fsep [ text "gesucht eine Ableitung"
                   , step_info, text ", die"
                   ]
            , nest 4 $ toDoc $ from inst
            ]
        peng $ from inst        
        inform $ vcat 
            [ text "überführt in"
            , nest 4 $ toDoc $ to inst
            ]
        peng $ to inst
        inform $ vcat
            [ text "Jeder Ableitungsschritt wird durch eine Nummer"
            , text "aus einer Liste von möglichen Schritten bezeichnet."
            ]
        
    initial p inst = [ 0 ]

    partial p inst xs = do
        result <- derive ( from inst ) xs
        return ()

    total p inst xs = do
        result <- silent $ derive ( from inst ) xs
        check_length inst xs
        check_result inst result


check_length inst xs = do
    case steps inst of
        Nothing -> return ()
        Just s -> assert ( s == length xs )
                         $ text "Ableitungslänge korrekt"
                           
check_result inst t = do
    assert ( to inst == t )
           $ text "Ableitungsergebnis korrekt?"

derive t xxs = do
    inform $ vcat 
        [ text "*****************************************************"
        , text "aktueller Term ist" 
        , nest 4 $ toDoc t 
        ]
    peng t
    let ps = redex_positions t
    inform $ vcat 
        [ text "Liste der Redex-Positionen ist"
        , nest 4 $ vcat $ map toDoc $ zip [ 0 :: Int .. ] ps
        ]
    case xxs of
        [] -> return t
        x : xs -> do
            inform $ text "Sie wählen den Redex Nummer" <+> toDoc x
            silent $ assert ( 0 <= x && x < length ps )
                   $ text "Nummer ist zulässig?"
            let p = ps !! x
            redex <- peek t p
            inform $ vcat [ text "Redex ist", nest 4 $ toDoc redex ]
            redukt <- step redex
            inform $ vcat [ text "Redukt ist", nest 4 $ toDoc redukt ]
            result <- poke t ( p, redukt )
            derive result xs

make :: Make
make = direct Derive I.example                    
