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

import qualified Data.Set as S
import Data.Set (Set)

import Data.Typeable

data CNF_Optimize = CNF_Optimize deriving ( Read, Show, Typeable )

instance OrderScore CNF_Optimize where
    scoringOrder _ = Increasing

instance Partial CNF_Optimize CNF CNF where

    describe _ i = vcat
             [ text "Gesucht ist eine Formel in konjunktiver Normalform,"
             , text "die äquivalent ist zu"
             , nest 4 $ toDoc i
             , text "und weniger Klauseln enthält als diese."
             , text "Die gesuchte Formel darf zusätzliche Variablen enthalten,"
             , text "über diese wird existentiell quantifiziert."
             ]

    initial _ i = i

    partial _ i b = do
        let extra = S.difference ( variables b ) ( variables i )
        when ( not $ S.null extra ) $ inform $ vcat
             [ text "zusätzliche Variablen"
             , nest 4 $ toDoc extra
             ]
        let mi = eval i
            mb = O.exists_many extra $ eval b
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


