{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-} 

module KnapsackFraction.Param where

import Autolib.Reader
import Autolib.ToDoc

import Data.Typeable

import Data.Ratio

import Autolib.Set
import Autolib.FiniteMap

data Param = 
     Param { anzahl      :: Int
	   , gewicht     :: ( Integer , Integer )
	   , wert        :: ( Integer , Integer )
	   , kapazitaet0 :: ( Integer , Integer )
	   }
     deriving ( Typeable )

p0 :: Param
p0 =  Param { anzahl      = 12
	    , gewicht     = (  3 , 12 )
	    , wert        = (  5 , 25 )
	    , kapazitaet0 = ( 40 , 60 )
	    }

$(derives [makeReader, makeToDoc] [''Param])

data Objekt = A | B | C | D | E | F | G | H | I | J | K | L | M 
	    | N | O | P | Q | R | S | T | U | V | W | X | Y | Z
	      deriving ( Eq , Ord , Read , Show , Typeable , Enum , Bounded )

$(derives [makeReader, makeToDoc] [''Objekt])

data Inp = 
     Inp { objekte         :: Set Objekt
	 , optimaler_wert  :: Rational
	 , kapazitaet      :: Integer
	 , gewichte        :: FiniteMap Objekt Integer
	 , werte           :: FiniteMap Objekt Integer
	 }
     deriving ( Typeable )

instance Container Rational (Integer, Integer) where
    label _ = "Rational"
    pack r = ( numerator r, denominator r )
    unpack ( n, d) = n % d

inp0 :: Inp
inp0 = Inp (mkSet [A,B,C,D,E,F])
	   (55%2)
	   (17)
	   (listToFM $ zip [A,B,C,D,E,F] [4,3,5,5, 9,6])
	   (listToFM $ zip [A,B,C,D,E,F] [6,4,9,8,12,9])

$(derives [makeReader, makeToDoc] [''Inp])

data Pack =
     Pack { pwert :: Rational
	  , teile :: FiniteMap Objekt Rational
	  }
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Pack])


-- local variables:
-- mode: haskell
-- end;
