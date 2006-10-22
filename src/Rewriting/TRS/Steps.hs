{-# OPTIONS -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Rewriting.Steps where

import Rewriting.Step
import Rewriting.TRS

import Type.Tree

import Autolib.TES.Apply ( apply )
import Autolib.TES.Unify ( match )

import Autolib.Schichten
import Autolib.Set
import Autolib.Reporter
import Autolib.ToDoc
import Autolib.FiniteMap

import Data.Maybe

-- | list all possible rewrite steps
-- starting from given term
steps :: ( Symbol v, Symbol c )
      => TRS v c
      -> Term v c
      -> [ Step v c ]
steps rs t = do
    ( p, s ) <- positions t
    ( k, r ) <- zip [ 0 .. ] $ regeln rs
    sub <- maybeToList $ match ( lhs r ) s
    return $ Step
           { rule_number = k
           , position = p
           , substitution = sub
           }

successors :: ( Symbol v, Symbol c )
      => TRS v c
      -> Term v c
      -> [ Term v c ]
successors rs t = do
    s <- steps rs t
    let Just t' = result $ exec rs t s
    return t'

data Derivation v c = 
    Derivation { goal :: Term v c
               , actions :: [ Step v c ]
               , start :: Term v c
               }

instance ( Symbol v, Symbol c ) => Eq ( Derivation v c ) where
    d == e = goal d == goal e

instance ( Symbol v, Symbol c ) => Ord ( Derivation v c ) where
    d `compare` e = goal d `compare` goal e


reachables ::  ( Symbol v, Symbol c )
          => TRS v c
          -> [ Term v c ]
          -> [ Set ( Derivation  v c )  ]
reachables trs ts = 
     let nach d = mkSet $ do
             let t = goal d
             step <- steps trs t
             maybeToList $ do
                 s <- result $ exec trs t step
                 return $ Derivation 
                        { goal = s
                        , actions = step : actions d
                        , start = start d
                        }
     in  schichten' nach 
             $ mkSet 
             $ map ( \ t -> Derivation { start = t, goal = t, actions = [] } )
             $ ts

-- | execute one rewrite step
exec ::  ( Symbol v, Symbol c )
     => TRS v c
     -> Term v c 
     -> Step v c 
     -> Reporter ( Term v c )
exec trs t step = do
    inform $ vcat
           [ text "Anwenden des Ersetzungsschrittes"
           , nest 4 $ toDoc step
           , text "auf den Term"
           , nest 4 $ toDoc t
           ]
    let k = rule_number step
    inform $ text "die Regel Nummer" <+> toDoc k
    rule <- if k < length ( regeln trs )
         then do
             let rule = regeln trs !! k
             inform $ text "ist" <+> toDoc rule
             return rule
         else reject $ text "existiert nicht."

    let p = position step
    inform $ text "der Teilterm an Position" <+> toDoc p 
    s <- case mpeek t ( position step ) of
         Just s -> do 
             inform $ text "ist" <+> toDoc s
             return s
         Nothing -> reject $ text "existiert nicht"

    let sub = substitution step
    inform $ text "die substituierte linke Regelseite ist" 
    slhs <- mapply sub $ lhs rule
    inform $ toDoc slhs

    assert ( slhs == s )
           $ text "stimmt überein mit Teilterm an Position?"

    inform $ text "die substituierte rechte Regelseite ist" 
    srhs <- mapply sub $ rhs rule
    inform $ toDoc srhs

    let res = poke t ( p,  srhs )
    inform $ text "der resultierende Term ist" 
           $$ ( nest 4 $ toDoc res )
    peng res

    return res

-------------------------------------------------------------------------

mpeek :: Term v c 
      -> Position 
      -> Maybe ( Term v c )
mpeek t [] = return t
mpeek ( Node f args ) (x : xs) = do
    guard $ x < length args
    mpeek ( args !! x ) xs
mpeek _ _ = Nothing

mapply :: ( Symbol v, Symbol w, Symbol c )
       => FiniteMap v ( Term w c)
       -> Term v c
       -> Reporter ( Term w c )
mapply sub ( Var v ) = do
    case lookupFM sub v of
        Just t -> return t
        Nothing -> reject $ fsep
            [ text "Variable", toDoc v, text "ist nicht gebunden" ]
mapply sub ( Node f args ) = do
    ys <- mapM ( mapply sub ) args
    return $ Node f ys
