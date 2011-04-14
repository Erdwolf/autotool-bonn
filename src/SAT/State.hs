-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-} 

module SAT.State where

--   $Id$

import SAT.Types

import Autolib.ToDoc
import Autolib.Reader

import Autolib.Set
import qualified Autolib.Relation as Relation
import Autolib.FiniteMap

data State = 
     State { assignment :: Belegung -- ^ erfüllend
	   , satisfying :: Set Literal -- ^ dazu gehörende literale
	   , todo :: Int -- ^ noch so viele klauseln
	   , width :: Int -- ^ soviele literale pro klausel
	   , formula  :: Formel -- ^ soweit schon gebaut
	   , clause :: Klausel -- ^ currently being built
	   , csat :: Bool -- ^ klausel ist bereits erfüllt?
	   , unfrequent :: Set Literal
	   , morefrequent :: Set Literal
	   , dependencies :: Relation.Type Variable Variable
	   }

$(derives [makeReader, makeToDoc] [''State])


