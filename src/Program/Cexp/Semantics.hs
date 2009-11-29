{-# language TemplateHaskell #-}

module Program.Cexp.Semantics where

import Program.Cexp.Type

import Data.Map ( Map )
import qualified Data.Map as M
import Autolib.FiniteMap

import Autolib.Reporter
import Autolib.ToDoc
import Data.Tree

data State = State { expression :: Exp
                   , store :: FiniteMap String Integer
                   }

$(derives [makeToDoc] [''State])

start :: Exp -> State
start x = State { expression = x, store = M.empty }

is_value :: Exp -> Bool
is_value x = case x of
    Apply {} -> False
    _ -> True

active_position :: Exp -> Position -> Reporter Expression
active_position x p = do
    inform $ text "Position" <+> toDoc p <+> text "in Ausdruck" <+> toDoc x
    case (x, p) of
        ( _, [] ) -> return x
        (Apply op args) (k : ks) -> do
            when ( k < 0 || length args >= k ) 
                 $ reject $ text "kein Kind mit Nummer" <+> toDoc k
            case strict op of
                Head -> when ( 0 /= k ) $ reject $ text "
            let ( pre, this : post ) = splitAt k args

