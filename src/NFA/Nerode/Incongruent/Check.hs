{-# LANGUAGE DeriveDataTypeable #-}
module NFA.Nerode.Incongruent.Check where

import NFA.Nerode.Incongruent.Instance
import NFA.Nerode.Incongruent.Solution

import Convert.Language

import Inter.Types
import Inter.Quiz
import Autolib.ToDoc
import Autolib.Hash

import qualified Challenger as C

import Data.Typeable
import Autolib.Reporter

import Autolib.NFA ( NFA, NFAC ) 
import Autolib.NFA.Example
import Autolib.NFA.Shortest
import Autolib.NFA.Minus

import Autolib.Exp.Inter

import Autolib.Size
import Autolib.FiniteMap
import Autolib.Informed

import Data.Ix
import Data.Maybe

import Prelude hiding ( words )

data Nerode_Incongruent = Nerode_Incongruent
    deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Nerode_Incongruent where
    scoringOrder _ = Decreasing

instance C.Verify Nerode_Incongruent ( Instance Char ) where
    verify Nerode_Incongruent i = do
        assert ( wanted i > 1 )
               $ text "wenigstens nach zwei Wörtern fragen"

instance C.Partial Nerode_Incongruent
                   ( Instance Char )
		   ( Solution Char )
  where

    describe Nerode_Incongruent i = vcat
        [ hsep [ text "Gesucht sind wenigstens"
               , toDoc ( wanted i )
               , text "verschiedene Wörter u_0, u_1, ...,"
               ]
	, text "die bezüglich der Sprache"
	, nest 4 $ nice $ language i
        , text "paarweise inkongruent sind."
	, text "Für jedes Paar (i,j) mit i < j"
	, text "sollen Sie ein Wort w angeben"
	, text "mit (u_i . w in L) <=> (u_j . w not in L)"
        ]

    initial Nerode_Incongruent i = 
        NFA.Nerode.Incongruent.Solution.example "ab"

    partial Nerode_Incongruent i s = do

        let d = min_det_automaton $ language i

        partial_check d s

    total Nerode_Incongruent i s = do
        let n = length $ words s
        assert ( wanted i <= n )
               $ text "genügend Wörter?"
	complete_map n $ proofs s

complete_map n p = sequence_ $ do
    i <- [ 0 .. n - 1 ]
    j <- [ i + 1 .. n - 1 ]
    let ij = (i,j)
    return $ when ( isNothing $ lookupFM p ij )
	   $ reject $ text "es fehlt der Beweis für das Indexpaar" <+> toDoc ij

partial_check d s = sequence_ $ do
    let bnd = ( 0, pred $ length $ words s )
    p @ ((i,j), w) <- fmToList $ proofs s
    return $ do
        inform $ text "betrachte Eintrag" <+> toDoc p
        let whine flag msg = silent $ assert flag $ text msg 
        whine (i < j) "erster Index soll kleiner als zweiter Index sein"
	whine (inRange bnd i) "erster Index soll im erlaubten Bereich liegen"
 	whine (inRange bnd j) "zweiter Index soll im erlaubten Bereich liegen"

        let ui = words s !! i ; uj = words s !! j
	    uiname = "u_" ++ show i ; ujname = "u_" ++ show j
	inform $ vcat [ text uiname <+> equals <+> toDoc ui
		      , text ujname <+> equals <+> toDoc uj
		      ]
	let handle tag v = do
		let p = Autolib.NFA.Shortest.is_accepted d v
                inform $ text tag <+> equals <+> toDoc v 
		  <+> ( if p then empty else text "not" )
		  <+> text "in L"
		return p
	pi <- handle ( uiname ++ " . w" ) $ ui ++ w
	pj <- handle ( ujname ++ " . w" ) $ uj ++ w
        when ( pi == pj ) $ reject 
	     $ text "diese Wahrheitswerte dürfen nicht übereinstimmen."
	      



instance NFAC c Int => C.Measure Nerode_Incongruent 
                   ( Instance c )
		   ( Solution c )
  where
    measure _ _ s = fromIntegral $ sum $ map length $ words s

make :: Make
make = direct Nerode_Incongruent 
              NFA.Nerode.Incongruent.Instance.example

