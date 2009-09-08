-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Machine.Numerical.Type where

import Autolib.Informed
import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter

import Data.Typeable

data Computer = Computer
     deriving Typeable

$(derives [makeReader, makeToDoc] [''Computer])

data Type m = 
     Make { fun ::  [ Integer ] -> Reporter Integer -- zu berechnende funktion
	  , fun_info :: Doc	      -- funktions-name/-erklärung
	  , extra_info :: Doc -- extra erklärung/bedingung
	  , args :: [[ Integer ]]	      -- argumente zum testen
	  , cut :: Int -- höchstens soviele schritte
	  , check :: m -> Reporter () -- sonstige Bedingungen an Maschine
	  , start :: m -- damit soll der student anfangen
	  }
     deriving Typeable

instance Informed ( Type m ) where
     info m = fun_info m
     informed info m = m { fun_info = info }

instance ToDoc ( Type m ) where
     toDoc = info

instance Reader ( Type m ) 

