{-# language DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}

module CNF.Optimize where

import CNF.Form
import CNF.Eval
import CNF.Example

import OBDD (OBDD)
import qualified OBDD.Operation as O
import qualified OBDD.Property as P

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Size

import Inter.Types
import Inter.Quiz

import Data.Typeable

data CNF_Optimize = CNF_Optimize deriving ( Read, Show, Typeable )

instance Partial CNF_Optimize CNF CNF where

    describe _ i = vcat
             [ text "Gesucht ist eine Formel in konjunktiver Normalform,"
             , text "die äquivalent ist zu"
             , nest 4 $ toDoc i
             , text "und kleiner ist als diese"
             ]

    initial _ i = i

    partial _ i b = do
        let mi = eval i
            mb = eval b
            diff = O.binary ( \ x y ->  x /= y ) mi mb
            mods = P.all_models diff
        when ( P.satisfiable diff ) $ reject $ vcat
             [ text "nicht äquivalent, z. B. bei Belegung(en)"
             , nest 4 $ vcat $ map toDoc $ take 3 mods
             ]

    total _ i b = do
        assert ( size i > size b )
               $ text "Formel ist kleiner?"

make_fixed :: Make
make_fixed = direct CNF_Optimize CNF.Example.e1


