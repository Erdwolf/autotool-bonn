{-# language DeriveDataTypeable, GADTs, MultiParamTypeClasses, FlexibleInstances, TemplateHaskell #-}

module Unify.Instance

( Instance (..)
, describe
)

where

import Prolog.Data
import Prolog.Substitution

import Autolib.ToDoc
import Autolib.Reader
import Autolib.FiniteMap
import Data.Typeable

data Instance = Instance
              { wildcard :: Identifier
              , left :: Term 
              , right :: Term
              , unifier :: Substitution
              }
    deriving ( Typeable )

$(derives [makeToDoc,makeReader] [''Instance])


describe :: Instance -> Doc
describe i = vcat
    [ text "Ersetzen Sie in dem Paar von Termen"
    , nest 4 $ hsep [ text "(t1, t2)", equals, toDoc ( left  i, right i ) ]
    , text "die Zeichen" <+> toDoc ( wildcard i ) <+> text "so durch Terme, daß"
    , nest 4 $ hsep [ text "sigma", equals, toDoc $ unifier i ]
    , text "ein allgemeinster Unifikator von t1 und t2 ist."
    ]

