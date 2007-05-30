{-# OPTIONS -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Pump.Inter2 where

import Pump.Type
import Pump.Positiv
import Pump.Negativ

import Language.Syntax
import Language.Inter
import Language
import Language.Sampler

import Pump.Conf2

import Challenger.Partial

import Autolib.Util.Sort
import Autolib.Util.Seed
import Autolib.Util.Zufall
import Autolib.Hash

import Autolib.FiniteMap
import Inter.Types
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

import qualified Pump.REG as REG
import qualified Pump.CF as CF

import Data.Typeable

------------------------------------------------------------------------

data PUMP = PUMP String
    deriving ( Eq, Ord, Typeable )

instance ToDoc PUMP where
    toDoc ( PUMP kind ) = text $ "PUMP-" ++ kind

instance Reader PUMP where
    reader = do
        my_reserved "PUMP"
        -- for backward compatibility
        cs <- option "REG" $ do
            Autolib.Reader.char '-'
            many alphaNum
        return $ PUMP cs

instance (  Hash ( Pump z ), Pumping z )
	 => Partial PUMP ( Conf z ) ( Pump z ) where

    describe ( PUMP {} ) ( conf :: Conf z ) = vcat 
	     [ text $ "Untersuchen Sie, ob die Sprache  L = "
	     , nest 4 $ toDoc (inter $ language $ sampler conf)
	     , text $ "die " ++ Pump.Type.tag ( undefined :: z ) ++ " erfüllt."
	     , text ""
	     , nest 4 $ parens
	              $ text "Ja-Einsendungen werden bei dieser Aufgabe"
	              $$ text "nur für  n <=" <+> toDoc (ja_bound conf)
	              <+> text "akzeptiert."
--	     , text "Zu dieser Sprache gehören unter anderem die Wörter:"
--	     , nest 4 $ toDoc $ take 10 $ samp conf
	     ]

    initial ( PUMP {} ) conf = 
        let ( yeah, noh ) = Language.Sampler.create 
                  ( sampler conf ) ( 314159 ) $ Just 10
            start = 3
	in  Ja { n = start, zerlege = listToFM $ do 
	           w <- positive_liste ( sampler conf ) start
		   return ( w, exem w )
	       }

    total   ( PUMP {} ) conf p @ ( Nein {} ) = do
	negativ ( inter $ language $ sampler conf ) p
	return ()

    total   ( PUMP {} ) conf p @ ( Ja   {} ) = do
	assert ( n p <= ja_bound conf ) 
	       $ text "Ist Ihr n kleiner als die Schranke?"
       	let ws = positive_liste ( sampler conf ) ( n p )
	positiv ( inter $ language $ sampler conf ) p ws
	return ()

positive_liste s n = 
        let ( yeah, noh ) = Language.Sampler.create s 314159 $ Just (2 * n)
        in  filter ( ( >= 2 * n) . length ) yeah

---------------------------------------------------------------------- 

reg = direct 
   ( PUMP "REG" ) ( Pump.Conf2.example :: Conf REG.Zerlegung )
cf  = direct 
   ( PUMP "CF"  ) ( Pump.Conf2.example :: Conf CF.Zerlegung )
