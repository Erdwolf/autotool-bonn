-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module Machine.Fun.Type where

--   $Id$

import Informed
import ToDoc
import Reader
import Reporter

data Computer = Computer

$(derives [makeReader, makeToDoc] [''Computer])

data Type m dat = 
     Make { pairs :: [ (dat, dat) ] -- zu berechnende funktion
	      , fun_info :: Doc	      -- funktions-name/-erklärung
	    , cut :: Int -- höchstens soviele schritte
	    , check :: m -> Reporter () -- sonstige Bedingungen an Maschine
	    , start :: m -- damit soll der student anfangen
	      }

instance Informed ( Type m dat ) where
     info = fun_info
     informed info m = m { fun_info = info }

instance ToDoc ( Type m dat ) where
     toDoc = info
instance Show ( Type m dat ) where
     show = render . toDoc

instance Reader ( Type m dat ) where
     reader = error "Machine.Fun.Type.reader"
instance Read ( Type m dat ) where
     readsPrec p = error "Machine.Fun.Type.readsPrec"
