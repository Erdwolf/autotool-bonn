{-# LANGUAGE TemplateHaskell #-}

module Machine.Numerical.Config where

import Arithmetic.Op

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter

import Data.Typeable

class  Check c m where
    check :: c -> m -> Reporter ()

class ( Reader [c], Reader m, Reader c
      , ToDoc [c], ToDoc m , ToDoc c
      , Typeable c, Typeable m
      )
    => ConfigC c m

instance  ( Reader [c], Reader m, Reader c
      , ToDoc [c], ToDoc m , ToDoc c
      , Typeable c, Typeable m
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
