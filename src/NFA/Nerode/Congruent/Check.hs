{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

module NFA.Nerode.Congruent.Check where

import NFA.Nerode.Congruent.Instance
import Convert.Language

import Inter.Types
import Inter.Quiz
import Autolib.ToDoc
import Autolib.Hash

import qualified Challenger as C

import Data.Typeable
import Autolib.Reporter

import Autolib.NFA ( NFA(..), NFAC ) 
import Autolib.NFA.Example
import Autolib.NFA.Shortest
import Autolib.NFA.Minus

import Autolib.Exp.Inter

import Autolib.Size
import Autolib.Set
import Autolib.Informed

import Data.Ix

data Nerode_Congruent = Nerode_Congruent
    deriving ( Eq, Ord, Show, Read, Typeable )

instance C.Verify Nerode_Congruent ( Instance Char ) where
    verify Nerode_Congruent i = do
        Convert.Language.verify $ language i
        assert ( wanted i > 1 )
               $ text "wenigstens nach zwei Wörtern fragen"

instance C.Partial Nerode_Congruent
                   ( Instance Char )
		   ( Set String )
  where

    describe Nerode_Congruent i = vcat
        [ hsep [ text "Gesucht sind wenigstens"
               , toDoc ( wanted i )
               , text "verschiedene Wörter w"
               ]
        , hsep [ text "mit"
               , toDoc $ minimal_length i
               , text "<= |w|"
               ]
	, text "die bezüglich der Sprache"
	, nest 4 $ nice ( language i )
        , text "kongruent sind zu"
        , nest 4 $ toDoc ( goal i )
        ]

    initial Nerode_Congruent i = 
        mkSet [ reverse $ goal i ]

    partial Nerode_Congruent i s = do
        let d = min_det_automaton $ language i 
        sequence_ $ do
            w <- setToList s
            return $ nested 4 $ do
                inform $ text "teste Wort w =" <+> toDoc w
                nested 4 $ assert ( minimal_length i <= length w )
                       $ text "Länge erlaubt?"
                nested 4 $ must_be_congruent d ( goal i ) w

    total Nerode_Congruent i s = do
        assert ( wanted i <= cardinality s )
               $ text "genügend Wörter?"

must_be_congruent d u v = do
    inform $ hsep 
           [ text "sind u =" , toDoc u
           , text "und v =", toDoc v
           , text "kongruent bezüglich der Sprache?"
           ]
    let section w = d { starts = reachables d w }
    let check_section (utag, u) (vtag, v) = do
            let diff = Autolib.NFA.Minus.minus_det 
                       ( section u ) (section v)
            case accepted diff of
                [ ] -> return ()
                w : _ -> reject $ vcat
                    [ text "Nein, für w =" <+> toDoc w <+> text "gilt"
                    , utag <+> text ". w in L"
                    , vtag <+> text ". w not in L"
                    ]
    check_section ( text "u", u) ( text "v", v)
    check_section ( text "v", v) ( text "u", u) 
    inform $ text "Ja."


instance C.Measure Nerode_Congruent 
                   ( Instance c )
		   ( Set [c] )
  where
    measure _ _ s = fromIntegral $ cardinality s

make :: Make
make = direct Nerode_Congruent 
              NFA.Nerode.Congruent.Instance.example
