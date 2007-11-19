module String_Matching.KMP.Central where

import String_Matching.Option
import String_Matching.KMP.Instance as I
import String_Matching.KMP.Config as C
import String_Matching.KMP.Semantics

import Challenger.Partial hiding ( No )
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter

import Autolib.Util.Zufall
import Autolib.Util.Sort
import Autolib.Set

import Inter.Types
import Inter.Quiz

import Data.Typeable

data String_Matching_KMP = String_Matching_KMP 
    deriving ( Read, Show, Typeable )

instance Measure String_Matching_KMP ( Instance a ) [a] where
    measure p i b = fromIntegral $ length b

instance ( Ord a, Reader a, ToDoc a ) 
    => Partial String_Matching_KMP ( Instance a ) [a] where

    describe p i = vcat
        [ text "Gesucht ist ein Wort, das mit seiner KMP-failure-function"
        , text "folgende Bedingungen erfüllt:"
        , nest 4 $ toDoc i
        , text "Ersetzen Sie die Fragezeichen!"
        ]

    initial p i = do
        let a : rest = setToList $ I.alphabet i
        x <- word i
        return $ case x of
            Yes x -> x
            No    -> a

    total p i b = do
        assert ( sub ( word i ) $ map Yes b )
               $ text "Wort paßt zum Muster?"
        let f = failure b
        inform $ vcat
               [ text "die failure-function Ihrer Eingabe ist"
               , nest 4 $ toDoc f 
               ]
        assert ( sub ( failures i ) $ map Yes f )
               $ text "paßt zum Muster?"

make_fixed :: Make 
make_fixed = direct String_Matching_KMP I.example

make_quiz :: Make
make_quiz = quiz String_Matching_KMP C.example

instance ( Ord a, Reader a, ToDoc a )
    =>  Generator String_Matching_KMP ( Config a ) ( Instance a ) where
    generator _ conf key = do
        bs <- sequence $ replicate ( take_best_of conf ) $ do
            w <- someIO ( setToList $ C.alphabet conf ) ( word_length conf )
            reduce ( C.alphabet conf ) w
        return $ head $ sortBy ( yes . word ) bs

instance ( Ord a, Reader a, ToDoc a )
    => Project String_Matching_KMP ( Instance a ) ( Instance a ) where
    project p = id

