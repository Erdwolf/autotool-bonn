{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Code.Compress where

--  $Id$

import Code.Type
import Code.Param
import Code.Quiz ( throw )
import Inter.Types
import Inter.Quiz

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Autolib.Reporter
import Autolib.Util.Zufall

import Data.Typeable

instance ( ToDoc c, Reader b, Coder c a b )
	 => Partial ( Compress c ) [ a ] b where

    describe ( Compress c ) i = vcat    
        [ text "Finden Sie eine möglichst gute Komprimierung von"
	, nest 4 $ toDoc i
	, text "nach dem Verfahren"
	, nest 4 $ toDoc c
        , text "d. h. eine möglichst kurze Nachricht,"
        , text "deren Dekompression wieder das Original ergibt."
	]

    initial ( Compress c ) i = encode c $ take 2 i

    total ( Compress c ) i b = do
        guess <- decodeR c b
	if ( i == guess ) 
	   then inform $ text "Das Ergebnis ist korrekt."
	   else reject $ text "Die Antwort ist nicht korrekt."
        let me  = measure c i $ encode c i 
            you = measure c i b
            bound = fromIntegral me * 1.1 -- FIXME: arbitrary
        inform $ vcat
               [ text "Es gibt eine Lösung der Größe" <+> toDoc me
               , text "Ihre Nachricht darf höchstens Größe" <+> toDoc bound 
                           <+> text "haben."
               , text "Ihre Nachricht hat die Größe" <+> toDoc you
               ]
        when ( fromIntegral you > bound ) $ reject $ text "Das ist zuviel."

instance ( Reader a , Read a, Reader [a]
	 , ToDoc c, Coder c a b, Size b ) 
     => Generator (Compress c) (Config a) [a] where
    generator (Compress c) conf key = do
        input <- throw conf `repeat_until` \ w ->
            measure c w (encode c w) < fromIntegral (length w ) 
	return input

instance Project (Compress c) [a] [a] where
    project _ = id

make_quiz :: ( ToDoc c, Reader b, Coder c Char b ) => c -> Make
make_quiz c = quiz (Compress c) Code.Param.example



