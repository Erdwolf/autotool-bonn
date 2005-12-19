{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}

module Rewriting.Overlap where

import Rewriting.TRS
import Autolib.TES.Unify

import Autolib.Size
import Autolib.ToDoc
import Autolib.Reader
import Autolib.FiniteMap

import Data.Typeable
import Data.Maybe

data ( Ord v, Symbol c ) => Overlap v c = Overlap
          { rule_one :: Int
          , rule_two :: Int
          , position_one :: Position
          , substitution :: FiniteMap (Either v v) (Term (Either v v) c)
          }
    deriving ( Eq, Ord, Typeable )

overlaps :: ( Symbol v, Symbol c )
         => TRS v c -> [ Overlap v c ]
overlaps trs = do
    ( i, rule1 ) <- zip [ 1.. ] $ regeln trs
    let x = vmap Left $ lhs rule1
    ( p, z ) <- positions x
    guard $ not $ isvar z

    ( j, rule2 ) <- zip [ 1 .. ] $ regeln trs
    let y = vmap Left $ lhs rule2
    -- treat self-overlaps specially
    guard $ ( i == j ) <= not ( null p )

    u <- maybeToList $ mgu z y

    return $ Overlap
           { rule_one = i
           , position_one = p
           , rule_two = j
           , substitution = u
           }

{-! for Overlap derive: Reader, ToDoc !-}

-- local variables:
-- mode: haskell
-- end;
