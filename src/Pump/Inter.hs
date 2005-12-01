{-# OPTIONS -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Pump.Inter where

--  $Id$

import Pump.Type
import Pump.Positiv
import Pump.Negativ

import Language.Syntax
import Language.Inter
import Language

import Pump.Conf
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


instance Pumping z => Partial PUMP ( Conf z ) ( Pump z ) where

    describe PUMP ( conf :: Conf z ) = vcat 
	     [ text $ "Untersuchen Sie, ob die Sprache  L = "
	     , nest 4 $ toDoc (inter $ lang conf)
	     , text $ "die " ++ Pump.Type.tag ( undefined :: z ) ++ " erfüllt."
	     , text ""
	     , nest 4 $ parens
	              $ text "Ja-Einsendungen werden bei dieser Aufgabe"
	              $$ text "nur für  n <=" <+> toDoc (ja_bound conf)
	              <+> text "akzeptiert."
	     , text "Zu dieser Sprache gehören unter anderem die Wörter:"
	     , nest 4 $ toDoc $ take 10 $ samp conf
	     ]

    initial PUMP conf = 
	    Ja { n = 3, zerlege = listToFM $ do 
	           w <- take 3 $ samp conf
		   return ( w, exem w )
	       }

    -- partial p conf z =

    total   PUMP conf p @ ( Nein {} ) = do
	negativ ( inter $ lang conf ) p
	return ()

    total   PUMP conf p @ ( Ja   {} ) = do
	assert ( n p <= ja_bound conf ) 
	       $ text "Ist Ihr n kleiner als die Schranke?"
	let ws = take 5 
	       $ filter ( (n p <= ) . length ) 
	       $ samp conf
	positiv ( inter $ lang conf ) p ws
	return ()

---------------------------------------------------------------------- 
 
instance ToDoc z => Generator PUMP ( Pump.Quiz.Conf z ) ( Conf z ) where
    generator p c key = do
        let l = Pump.Quiz.lang c
      	wss <- sequence $ do
	    c <- [ 10 .. 30 ]
	    return $ samples ( inter l ) 2 c
	return $ Conf { lang = l
			    , samp = nub $ concat wss
			    , ja_bound = Pump.Quiz.ja_bound c
			    }

instance Project PUMP ( Conf z ) ( Conf z ) where
    project p c = c

---------------------------------------------------------------------- 

-- | name is for backward compatibility
reg = named_quiz "PUMP-Quiz" PUMP 
      ( Pump.Quiz.example :: Pump.Quiz.Conf REG.Zerlegung )

cf  = named_quiz "Pump-CF" PUMP 
      ( Pump.Quiz.example :: Pump.Quiz.Conf CF.Zerlegung )
