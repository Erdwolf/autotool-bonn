{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, TypeSynonymInstances, DeriveDataTypeable, UndecidableInstances, TemplateHaskell #-}

module Machine.Acceptor.Type where

--   $Id$

import Autolib.Informed
import Autolib.ToDoc
import Autolib.Reader
import qualified Autolib.Reporter.Checker as C
import Autolib.Reporter hiding ( output )

import Machine.Class
import Inter.Types ( ScoringOrder (..) , OrderScore (..) )

import Data.Typeable

data Acceptor = Acceptor String -- include machine type ??
    deriving Typeable

instance OrderScore Acceptor where
    scoringOrder _ = Increasing

instance ToDoc Acceptor where
    toDoc ( Acceptor kind ) = text $ "Acceptor-" ++ kind

instance Reader Acceptor where
    reader = do
        my_reserved "Acceptor"
        Autolib.Reader.char '-'
        cs <- many alphaNum
        return $ Acceptor cs

class ( Reader [dat], ToDoc [dat], Reader [prop], ToDoc [prop]  ) 
      => Class dat prop 

instance ( Reader [dat], ToDoc [dat], Reader [prop] , ToDoc [prop]   ) 
      => Class dat prop 

data Class dat prop => Type m dat prop = 
     Make { machine_desc :: Doc
	  , data_desc :: Doc
	  , yeah :: [ dat ] -- ^ sollen akzeptiert werden
	  , noh :: [ dat ] -- ^ dürfen nicht akzeptiert werden
	    , cut :: Int -- ^ höchstens soviele schritte
	    , properties :: [ prop ] -- ^ sonstige Bedingungen an Maschine
	    , start :: m -- ^ damit soll der student anfangen
	      }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Type])

-- local variables:
-- mode: haskell
-- end:
