{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, UndecidableInstances, FlexibleContexts #-} 

module Convert.Language where

import Convert.Input
import Autolib.NFA ( NFAC )

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter
import Data.Typeable

import qualified Autolib.NFA
import qualified Autolib.Exp


data NFAC c Int => Language c = Language
        { implementation :: Input c
	, description :: Maybe String
	}
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Language])

example :: Language Char
example = Language
	{ implementation = Convert.Input.example
	, description = Nothing
	}

instance NFAC c Int =>  Nice ( Language c ) where
    nice l = case description l of
        Just cs -> text cs
	Nothing -> lang $ implementation l

verify ::  NFAC c Int => Language c -> Reporter ()
verify = verify_source . implementation

min_det_automaton :: Autolib.NFA.NFAC c Int 
		  => Language c -> Autolib.NFA.NFA c Int
min_det_automaton = Convert.Input.min_det_automaton . implementation

-- local variables:
-- mode: haskell
-- end
