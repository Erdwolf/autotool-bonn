-- -*- mode: haskell -*-
{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, FlexibleContexts #-} 

module Pump.Type where

--   $Id$

import Autolib.ToDoc
import Autolib.Reader

import Autolib.Reporter

import Autolib.Size
import Autolib.Hash
import Autolib.FiniteMap

import Data.Typeable

--------------------------------------------------------------------------

class ( Hash z, Ord z,  Show z, ToDoc z, ToDoc [z], Reader z, Typeable z ) 
       => Pumping z where
    tag :: z -> String
    tag_show :: z -> String
    admissable :: Int -> z -> Reporter ()
    inflate_show :: Int -> z -> String
    inflate_show_i :: z -> String

    inflate :: Int -> z -> String
    zerlegungen :: String -> Int -> [ z ]

    exempel :: z
    exem :: String -> z

--------------------------------------------------------------------------

data Pumping z => 
     Pump z = Nein
	     { wort :: FiniteMap Int String }
	    | Ja
	     { n :: Int
	     , zerlege :: FiniteMap String z
	     }
    deriving ( Eq, Typeable )

$(derives [makeReader, makeToDoc] [''Pump])

instance Pumping z => Size ( Pump z ) where
    size p @ Nein {} = sum [ length $ w | (n,w) <- fmToList $ wort p ]
    size p @ Ja   {} = n p

instance Pumping z => Hash ( Pump z ) where
    hash ( n @ Nein {} ) = hash $ wort n
    hash ( j @ Ja {}   ) = hash ( n j, hash $ zerlege j )

