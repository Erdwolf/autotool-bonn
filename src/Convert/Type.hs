{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

{-# LANGUAGE TemplateHaskell #-}
module Convert.Type where

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

import qualified Convert.Input

import Autolib.Exp.Inter
import Autolib.Exp
import Autolib.NFA hiding ( alphabet )
import Autolib.Set

data Convert =
     Convert { name :: Maybe [String] -- if Nothing, use (show input)
             , input :: Convert.Input.Input Char
	     }
    deriving ( Typeable , Show )

$(derives [makeReader, makeToDoc] [''Convert])
-- {-! for Convert derive: Reader, ToDoc !-}

form :: Convert -> Doc
form conv = case name conv of
    Nothing  -> Convert.Input.lang $ input conv
    Just css -> vcat $ map text css

eval :: Set Char -> Convert -> NFA Char Int
eval alpha conv = case input conv of
    Convert.Input.NFA aut -> aut
    Convert.Input.Exp exp -> inter ( std_sigma $ setToList alpha ) exp

-- local variables:
-- mode: haskell;
-- end;

