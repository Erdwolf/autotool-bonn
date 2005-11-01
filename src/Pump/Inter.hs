{-# OPTIONS -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Pump.Inter where

-- -- $Id$

import Pump.Type
import Pump.Positiv
import Pump.Negativ

import Language.Syntax
import Language.Inter
import Language

import qualified Pump.Quiz

import Challenger.Partial

import Autolib.Util.Sort
import Autolib.Util.Seed

import Autolib.FiniteMap
import Inter.Types
import Autolib.Reporter
import Autolib.ToDoc

import qualified Pump.REG as REG
import qualified Pump.CF as CF

import Inter.Quiz

import Data.Typeable

------------------------------------------------------------------------

data PUMP = PUMP deriving ( Eq, Ord, Show, Read, Typeable )

data Conf z = Conf { lang :: Language
		 , samp :: [ String ]
		 , typo :: z -- unused, nur zur Typ-information
		 , ja_bound :: Int
		 }
     deriving Typeable

instance Pumping z => Partial PUMP ( Conf z ) ( Pump z ) where
    describe PUMP conf = vcat 
	     [ text $ "Untersuchen Sie, ob die Sprache  L = "
	     , nest 4 $ toDoc (lang conf)
	     , text $ "die " ++ Pump.Type.tag (typo conf) ++ " erfüllt."
	     , text ""
	     , nest 4 $ parens
	              $ text "Ja-Einsendungen werden bei dieser Aufgabe"
	              $$ text "nur für  n <=" <+> toDoc (ja_bound conf)
	              <+> text "akzeptiert."
	     , text "Zu dieser Sprache gehören unter anderem die Wörter:"
	     , nest 4 $ toDoc $ take 10 $ samp conf
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
	assert ( n p <= ja_bound conf ) 
	       $ text "Ist Ihr n kleiner als die Schranke?"
	let ws = take 5 
	       $ filter ( (n p <= ) . length ) 
	       $ samp conf
	positiv ( lang conf ) p ws
	return ()

---------------------------------------------------------------------- 

instance Generator PUMP ( Pump.Quiz.Conf z ) ( Conf z ) where
    generator p c key = do
        let l = Language.Inter.inter $ Pump.Quiz.lang c
      	wss <- sequence $ do
	    c <- [ 10 .. 30 ]
	    return $ samples l 2 c
	return $ Conf { lang = l
			    , samp = nub $ concat wss
			    , typo = undefined 
			    , ja_bound = 5
			    }

instance Project PUMP ( Conf z ) ( Conf z ) where
    project p c = c

---------------------------------------------------------------------- 

reg = quiz PUMP ( Pump.Quiz.example :: Pump.Quiz.Conf REG.Zerlegung )
cf  = quiz PUMP ( Pump.Quiz.example :: Pump.Quiz.Conf REG.Zerlegung )
