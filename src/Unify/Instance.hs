{-# language DeriveDataTypeable, GADTs, MultiParamTypeClasses, FlexibleInstances, TemplateHaskell #-}

module Unify.Instance

( module Unify.Instance.Data 
, describe
)

where

import Unify.Instance.Data

import Autolib.ToDoc

describe :: InstanceC v c => Instance v c -> Doc
describe i = vcat
    [ text "Ersetzen Sie in dem Paar von Termen"
    , nest 4 $ hsep [ text "(t1, t2)", equals, toDoc ( left  i, right i ) ]
    , text "die Zeichen" <+> toDoc ( wildcard i ) <+> text "so durch Terme, daß"
    , nest 4 $ hsep [ text "sigma", equals, toDoc $ unifier i ]
    , text "ein allgemeinster Unifikator von t1 und t2 ist."
    ]

