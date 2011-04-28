{-# LANGUAGE UndecidableInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
module Code.Compress where

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
import Data.IORef
import Data.Maybe
import Data.Ratio

instance ( ToDoc c, Reader c, Reader a, ToDoc a, Coder c a b, BitSize b 
         )
	 => Partial ( Compress c ) [ a ] b where

    describe ( Compress c ) i = vcat    
        [ text "Finden Sie eine möglichst gute Komprimierung von"
	, nest 4 $ toDoc i
	, text "nach dem Verfahren"
	, nest 4 $ toDoc c
        , text "d. h. eine möglichst kurze Nachricht,"
        , text "deren Dekompression wieder das Original ergibt."
        , let me  = bitSize ( encode c i ) :: Integer
              bound = ( fromIntegral me :: Double ) * 1.1 -- FIXME: arbitrary
          in nest 4 $ vcat
                    [ text "Es gibt eine Lösung der Bit-Größe" <+> toDoc me
                    , text "Ihre Nachricht darf höchstens Bit-Größe"
                               <+> toDoc bound <+> text "haben."
                    ]
	]

    initial ( Compress c ) i = encode c $ take 2 i

    total ( Compress c ) i b = do
        guess <- decodeR c b
	if ( i == guess ) 
	   then inform $ text "Das Ergebnis ist korrekt."
	   else reject $ text "Die Antwort ist nicht korrekt."
        let me  = bitSize $ encode c i 
            bound = fromIntegral me * 1.1 -- FIXME: arbitrary
            you = bitSize b
        inform $ text "Ihre Nachricht hat die Größe" <+> toDoc you
        when ( fromIntegral you > bound ) $ reject $ text "Das ist zuviel."

instance BitSize b => Measure ( Compress c ) [ a ] b  where
    measure ( Compress c ) xs b = bitSize b

instance OrderScore ( Compress c ) where
    scoringOrder _ = Increasing

make_fixed :: ( ToDoc c,  Reader c, Reader b, Coder c Char b ) => c -> Make
make_fixed c = direct ( Compress c ) "01001010010010100101001001010010"


instance ( Reader a , Read a, Reader [a]
	 , ToDoc c, Reader c, Coder c a b, Size b ) 
     => Generator (Compress c) (Config a) [a] where
    generator (Compress c) conf key = do
        best_w <- newIORef []
        best_k <- newIORef Nothing
        let action =  do
               w <- throw conf 
               let a = bits $ cardinality $ mkSet w
               let k = fromIntegral (a * length w) % bitSize (encode c w) 
               k0 <- readIORef best_k
               when ( k > 1 && Just k > k0 ) $ do
                   writeIORef best_k $ Just k
                   writeIORef best_w $ w
            block = ( do 
               sequence_ $ replicate 1000 $ action
               readIORef best_k
              ) `repeat_until` \ bk -> isJust bk
        block     
        readIORef best_w
        

instance Project (Compress c) [a] [a] where
    project _ = id


make_quiz :: ( ToDoc c, Reader c, Reader b, Coder c Char b ) => c -> Make
make_quiz c = quiz (Compress c) Code.Param.example




