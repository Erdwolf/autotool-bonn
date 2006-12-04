module Flow.Action where

import Autolib.TES.Identifier

-- | the alphabet for the automaton
data Type
    = Statement Id
    | Condition Id Bool

