module Grammatik.CF.Akzeptor where

--  $Id$

import Grammatik.Type

import qualified Grammatik.CF.Chomsky as C

import Grammatik.Reduziert
import Grammatik.CF.DPL_CYK


akzeptor :: Grammatik -> ( String -> Bool )
akzeptor g = 
    let ch = C.make g
    in	accepted ch

