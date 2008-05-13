{-# language MultiParamTypeClasses, DeriveDataTypeable #-}

module Diffie_Hellman.Break where

import Diffie_Hellman.Config
import Prime.Check

import Data.Typeable

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Ana

import Inter.Types

data Diffie_Hellman_Code_Break = Diffie_Hellman_Code_Break deriving ( Read, Show, Typeable )

instance Verify Diffie_Hellman_Code_Break Config where
    verify Diffie_Hellman_Code_Break conf =  do
        let peh = p $ public conf 
            geh = g $ public conf
            ah  = a $ private conf
            beh = b $ private conf
        assert ( is_primitive_root peh geh )
               $ text "ist g eine primitive Wurzel für Modul p?"
        assert ( powMod geh ah  peh == g_a ( public conf ) )
               $ text "ist der Parameter g_a korrekt?"
        assert ( powMod geh beh peh == g_b ( public conf ) )
               $ text "ist der Parameter g_b korrekt?"
        assert ( powMod geh ( ah * beh ) peh == g_ab ( private conf ) )
               $ text "ist der Parameter g_ab korrekt?"

instance Partial Diffie_Hellman_Code_Break Config Integer where

    describe Diffie_Hellman_Code_Break conf = vcat
	   [ text "Finden Sie die Schlüssel für ein Diffie_Hellman-Protokoll mit"
	   , nest 4 $ toDoc $ public conf
	   ]

    initial Diffie_Hellman_Code_Break conf  = 
        let u = public conf
        in  ( g_a u * g_b u ) `mod` ( p u )

    total Diffie_Hellman_Code_Break conf k = do
	assert ( g_ab ( private conf ) == k )
	       $ text "korrekt?"

instance Measure Diffie_Hellman_Code_Break Config Integer where
    measure Diffie_Hellman_Code_Break c i = 0


make :: Make
make = direct Diffie_Hellman_Code_Break Diffie_Hellman.Config.example

