{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fallow-overlapping-instances #-}

{-# LANGUAGE TemplateHaskell #-}
module Machine.Numerical.Config where

--   $Id$

import Arithmetic.Op

import Autolib.Informed
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter

import Data.Typeable
-- import Text.XML.HaXml.Haskell2Xml

class  Check c m where
    check :: c -> m -> Reporter ()

class ( Reader [c], Reader m, Reader c
      , ToDoc [c], ToDoc m , ToDoc c
      , Typeable c, Typeable m
      -- , Haskell2Xml [c], Haskell2Xml m, Haskell2Xml c
      )
    => ConfigC c m

instance  ( Reader [c], Reader m, Reader c
      , ToDoc [c], ToDoc m , ToDoc c
      , Typeable c, Typeable m
      -- , Haskell2Xml [c], Haskell2Xml m, Haskell2Xml c
      )
    => ConfigC c m

data  ConfigC c m => Config c m = 
     Config { name :: String
	    , conditions :: [String] -- extra bedingungen
	    , arity :: Int
	    , op :: Exp Integer -- zu berechnende funktion
	      , num_args :: Int	      -- zum Testen: anzahl 
	      , max_arg :: Integer 
	    , cut :: Int -- höchstens soviele schritte
	    , checks :: [c]  -- sonstige Bedingungen an Maschine
	    , start :: m -- damit soll der student anfangen
	      }
     deriving ( Eq, Ord, Typeable )

$(derives [makeReader, makeToDoc] [''Config])
-- {-! for Config derive: Reader, ToDoc, Haskell2Xml !-}

