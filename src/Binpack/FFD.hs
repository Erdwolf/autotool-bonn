{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveDataTypeable #-}

module Binpack.FFD where


import Binpack.Instance
import Binpack.Example
import qualified Binpack.Param as P
import Binpack.Approximation

import Binpack.Interface ()
import Binpack.InstanceTH ()
import Binpack.ParamTH ()

import Autolib.FiniteMap
import Autolib.Set

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Util.Zufall

import Inter.Types

import Data.List ( sort )
import Data.Typeable



data Binpack_FFD = Binpack_FFD 
    deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Binpack_FFD where
    scoringOrder _ = Increasing

instance Partial Binpack_FFD P.Param ( Instance, Assignment ) where

    describe _ p = vcat
          [ text "geben Sie eine Binpack-Instanz (mit Lösung) mit diesen Parametern an:"
          , nest 4 $ toDoc p
          , text "für die first-fit-decreasing zuviele Behälter verbraucht."
          ]

    initial _ p = 
        let c = Instance { bins = P.bins p, capacity = P.capacity p
                         , weights = replicate ( P.bins p ) ( P.capacity p )
                         }
        in  ( c, first_fit c )

    partial _ p ( i, s )  = do
        inform $ text "Die Instanz ist gültig?"
        nested 4 $ verify Binpack i

        inform $ text "Die Instanz paßt zu den Parametern?"
        nested 4 $ do
            assert ( bins i == P.bins p ) $ text "Anzahl der Behälter"
            assert ( capacity i == P.capacity p ) $ text "Kapazität der Behälter"

        inform $ text "Die Lösung paßt zur Instanz?"
        nested 4 $ do
            partial Binpack i s
            total   Binpack i s

    total _ p ( i, s ) = do
        let s' = first_fit_decreasing i
        inform $ vcat
               [ text "Der Algorithmus first-fit-decreasing liefert"
               , nest 4 $ toDoc s'
               ]
        assert ( length s' > bins i ) $ text "benutzt mehr Behälter?"

instance Verify Binpack_FFD P.Param where
    verify _ p = do
        assert ( P.bins p > 0 ) $ text "bins > 0"
        assert ( P.capacity p > 0 ) $ text "capacity > 0"


instance Measure Binpack_FFD P.Param (Instance, Assignment) where
    measure _ p (i, s) = 
        let s' = first_fit_decreasing i
        in  fromIntegral $ length s' - bins i
             
make_fixed :: Make
make_fixed = direct Binpack_FFD P.example








