{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses #-}

module Grammatik.Akzeptor where

import Language.Type
import Language.Inter

import Grammatik.Type
import Grammatik.Property
import Grammatik.Machine
import Grammatik.Config as GC

import qualified Machine.Acceptor.Type as A
import qualified Machine.Acceptor.Inter


import Inter.Types
import Inter.Quiz

import Autolib.ToDoc
import Autolib.Informed
import Autolib.Util.Zufall


acceptor :: Make
acceptor = quiz ( A.Acceptor "Grammatik" ) GC.example

type Accept = A.Type Grammatik String Property

instance Project A.Acceptor Accept Accept where
    project _ i = i

instance Generator A.Acceptor GC.Config Accept where
    generator _ config key = do
        let l = inter $ GC.lang config
            m = GC.max_num config
            e = GC.max_length config
            small = \ w -> length w <= e
        yeah <- lift $ samples      l m 0
        noh  <- lift $ anti_samples l m 0
        return $ A.Make
           { A.machine_desc = text "Grammatik (als Akzeptor)"
           , A.data_desc = info $ GC.lang config
           , A.yeah = take m $ filter small yeah
           , A.noh  = take m $ filter small noh
           , A.cut  = GC.cut config
           , A.properties = Typ 0 : Monoton : GC.properties config
           , A.start = GC.start config
           }
