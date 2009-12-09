{-# language TemplateHaskell #-}

module Flow.Action where

import Autolib.TES.Identifier
import Autolib.Symbol


import Autolib.ToDoc
import Autolib.Reader
import Autolib.Size
import Autolib.Hash


-- | used in the alphabet for the automaton
data Action
    = Execute Identifier
    | Halt
  deriving ( Eq, Ord )

$(derives [makeToDoc,makeReader] [''Action])

instance Symbol Action -- ?
instance Size Action where size = const 1
instance Hash Action where 
    hash a = case a of
        Execute i -> hash ( 42 :: Int, i )
        Halt -> 24
