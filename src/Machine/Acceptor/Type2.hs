{-# LANGUAGE TemplateHaskell #-}

module Machine.Acceptor.Type2 where

--   $Id$

import Language.Sampler

import Autolib.Informed
import Autolib.ToDoc
import Autolib.Reader
import qualified Autolib.Reporter.Checker as C
import Autolib.Reporter hiding ( output )

import Machine.Class

import Data.Typeable

data Acceptor = Acceptor String -- include machine type ??
    deriving Typeable

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
     Make { machine_desc :: String
            , source :: Sampler
	    , cut :: Int -- ^ höchstens soviele schritte
	    , properties :: [ prop ] -- ^ sonstige Bedingungen an Maschine
	    , start :: m -- ^ damit soll der student anfangen
	      }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Type])

-- local variables:
-- mode: haskell
-- end:
