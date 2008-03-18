{-# OPTIONS -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Rewriting.SRS.Steps where

import Rewriting.SRS.Step
import Rewriting.SRS.Raw

import Autolib.TES.Rule
import Autolib.Symbol

import Autolib.Reporter
import Autolib.ToDoc

import Data.List ( inits, tails )
import Data.Maybe

-- | list all possible rewrite steps
-- starting from given term
steps :: (  Symbol c )
      => SRS  c
      -> [c]
      -> [ Step c ]
steps rs w = do
    ( p, rest ) <- zip [0..] $ tails w
    ( k, r ) <- zip [ 0 .. ] $ regeln rs
    let ( pre, post ) = splitAt ( length $ lhs r ) rest
    guard $ lhs r == pre
    return $ Step
           { rule_number = k
           , position = p
           }


-- | execute one rewrite step
exec ::  ( Symbol c )
     => SRS c
     -> [ c ] 
     -> Step c 
     -> Reporter [ c ]
exec srs w step = do
    inform $ vcat
           [ text "Anwenden des Ersetzungsschrittes"
           , nest 4 $ toDoc step
           , text "auf das Wort"
           , nest 4 $ toDoc w
           ]
    let k = rule_number step
    inform $ text "die Regel Nummer" <+> toDoc k
    rule <- if k < length ( regeln srs )
         then do
             let rule = regeln srs !! k
             inform $ text "ist" <+> toDoc rule
             return rule
         else reject $ text "existiert nicht."

    let p = position step
        ( pre, midpost ) = splitAt p w
    inform $ text "das Teilwort an Position" <+> toDoc p 
    if p > length w 
         then reject $ text "existiert nicht"
         else inform  $ text "ist" <+> toDoc midpost

    let ( mid, post ) = splitAt ( length $ lhs rule ) midpost
    assert ( mid == lhs rule )
           $ text "linke Regelseite ist Präfix des Teilwortes an Position?"
    inform $ text "Suffix ist" <+> toDoc post

    let res = pre ++ rhs rule ++ post
    inform $ text "resultierendes Wort ist" 
           $$ ( nest 4 $ toDoc res )

    return res

