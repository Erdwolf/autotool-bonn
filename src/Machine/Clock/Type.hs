-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module Machine.Clock.Type where

--   $Id$

import Informed
import ToDoc
import Reader
import Reporter

data Clock = Clock

$(derives [makeReader, makeToDoc] [''Clock])

data Type m = 
     Make { fun ::  Integer -> Integer -- gewünschte Laufzeitfunktion
	      , fun_info :: Doc	      -- funktions-name/-erklärung
	      , args :: [ Integer ]	      -- argumente zum testen
	    , cut :: Int -- höchstens soviele schritte
	    , check :: m -> Reporter () -- sonstige Bedingungen an Maschine
	    , start :: m -- damit soll der student anfangen
	      }

instance Informed ( Type m ) where
     info = fun_info
     informed info m = m { fun_info = info }

instance ToDoc ( Type m ) where
     toDoc = info
instance Show ( Type m ) where
     show = render . toDoc

instance Reader ( Type m ) where
     reader = error "Machine.Clock.Type.reader"
instance Read ( Type m ) where
     readsPrec p = error "Machine.Clock.Type.readsPrec"
