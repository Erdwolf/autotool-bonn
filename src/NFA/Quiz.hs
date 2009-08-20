{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

{-# LANGUAGE TemplateHaskell #-}
module NFA.Quiz where

import Autolib.NFA

import qualified NFA.Property as A
import qualified Exp.Property as E

import Autolib.Reader
import Autolib.Symbol
import Autolib.ToDoc
import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

data NFAC c Int => From c = From_Exp [ E.Property c ]
	    | From_NFA [ A.Property c ]
    deriving ( Typeable )

$(derives [makeToDoc] [''From])
-- {-! for From derive: ToDoc, Haskell2Xml !-}

-- damit wir die alten Records noch lesen können
instance ( Reader [c], Symbol c ) => Reader ( From c ) where
-- NOTE/HACK: ohne Reader[c] gehen Strings nicht mit Doppelquotes
    reader = do my_reserved "From_Exp" ; fmap From_Exp reader
         <|> do my_reserved "From_NFA" ; fmap From_NFA reader
	 <|> do {- früher -}             fmap From_Exp reader

data NFAC c Int => 
     Quiz c = Quiz { generate :: From c
		 , solve    :: [ A.Property c ]
		 }
    deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Quiz])
-- {-! for Quiz derive: Reader, ToDoc, Haskell2Xml !-}

example :: Quiz Char
example = Quiz { generate = From_Exp [ E.Alphabet $ mkSet "ab"
			    , E.Max_Size 10
			    , E.Simple
			    ]
	       , solve    = [ A.Alphabet $ mkSet "ab"
			    ]
	       }

-- local variables;
-- mode: haskell;
-- end;
