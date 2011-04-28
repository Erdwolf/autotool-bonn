{-# LANGUAGE UndecidableInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Machine.Acceptor.Inter2 where

import qualified Machine.Acceptor.Type2 as A
import Machine.Akzeptieren
import Machine.Class
import Condition

import Language.Sampler
import Language.Inter
import Language.Type

import qualified Challenger as C

import Inter.Types
import Autolib.Reporter hiding ( output )
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Hash
import Autolib.Informed

instance ( Condition prop m , Reader m, Hash m
         ,  Machine m String conf 
         , ToDoc [prop]  , ToDoc prop
	 , Reader [prop]  , Reader prop
	 ) 
         => C.Partial ( A.Acceptor ) ( A.Type m String prop ) m where
    describe p i  = vcat
	[ text "Gesucht ist ein" <+> text ( A.machine_desc i )
        , text "für die Sprache"
        , nest 4 $ toDoc $ inter $ language $ A.source i
        , text "über dem Alphabet"
                   <+> toDoc ( alphabet $ inter $ language $ A.source i )
        , text ""
	, text "mit diesen Eigenschaften" <+> toDoc (A.properties i)
	]

    initial p i   = A.start i
    partial p i b = investigate ( A.properties i ) b
    total   p i b = do
        let ( yeahs, nohs ) = Language.Sampler.create ( A.source i )
                            ( hash b ) Nothing
        positiv_liste (A.cut i) b $ yeahs
        negativ_liste (A.cut i) b $ nohs
        return () 


