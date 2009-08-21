--  -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell #-}

module NFA.SI where

import Autolib.Exp
import qualified Autolib.Exp.Example as E
import Autolib.Set
import Autolib.ToDoc
import Autolib.Reader

import Data.Typeable

data SI = SI { name :: String -- abkürzung
	     , ausdruck :: Exp   
	     , alphabet :: Set Char
	     , beschreibung :: Maybe String -- falls Nothing, dann ausdruck
	     , deterministisch :: Bool      -- soll deterministisch sein?
	     }
          deriving ( Typeable )

example :: SI
example = 
    let sigma = mkSet "abc"
    in  SI { name = "irgendeine Sprache"
	   , beschreibung = Nothing
	   , alphabet = sigma
	   , ausdruck = E.example sigma
	   , deterministisch = True
	   }

$(derives [makeReader, makeToDoc] [''SI])

instance  Show SI where show = render . toDoc
instance  Read SI where readsPrec = parsec_readsPrec