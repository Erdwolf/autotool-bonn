module Pump.Inter where

-- $Id$

import Pump.Type
import Pump.Positiv
import Pump.Negativ

import Language.Type

import Challenger.Partial

import Util.Sort
import Util.Seed

import Data.FiniteMap
import Inter.Types
import Reporter
import ToDoc

------------------------------------------------------------------------

data PUMP = PUMP deriving ( Eq, Ord, Show, Read )

data Conf z = Conf { lang :: Language
		 , samp :: [ String ]
		 , typo :: z -- unused, nur zur Typ-information
		 }
     deriving Show -- ??

instance Pumping z => Partial PUMP ( Conf z ) ( Pump z ) where
    describe PUMP conf = vcat $ map text
	     [ "Untersuchen Sie, ob die Sprache  L = "
	     , show (lang conf)
	     , "die " ++ tag (typo conf) ++ " erfüllt."
	     ]
    initial PUMP conf = 
	let e = exempel
	    w = inflate 1 e
	in  Ja { n = 3, zerlege = listToFM [ (w, e) ] }

    -- partial p conf z =

    total   PUMP conf p @ ( Nein {} ) = do
	negativ ( lang conf ) p
	return ()

    total   PUMP conf p @ ( Ja   {} ) = do
	let ws = take 5 
	       $ filter ( (n p <= ) . length ) 
	       $ samp conf
	positiv ( lang conf ) p ws
	return ()

 
type VP z = Var PUMP ( Conf z ) ( Pump z )

make :: Pumping z
     => Language
     -> VP z
make l = 
    Var { problem = PUMP
	, aufgabe = "P"
	, version = nametag l
	, key = \ matrikel -> return matrikel
	, gen = \ key -> do
	      seed $ read key
	      -- TODO: cache this:
	      wss <- sequence $ do
	           c <- [ 0 .. 15 ]
	           return $ samples l 2 c
	      return $ return 
	             $ Conf { lang = l
			    , samp = nub $ concat wss
			    , typo = undefined :: z
			    }
	}

