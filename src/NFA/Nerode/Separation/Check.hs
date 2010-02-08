module NFA.Nerode.Separation.Check where

import NFA.Nerode.Separation.Solution

import qualified NFA.Nerode.Incongruent.Check as C
import qualified NFA.Nerode.Incongruent.Solution as S

import qualified Exp.Test
import qualified Exp.Property

import Inter.Types
import Inter.Quiz
import Autolib.ToDoc
import Autolib.Hash
import Autolib.Set

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
import Data.List ( nub )

import Prelude hiding ( words )

data Nerode_Separation = Nerode_Separation
    deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Nerode_Separation where
    scoringOrder _ = Increasing

instance C.Partial Nerode_Separation
                   [ String ]
		   ( Solution Char )
  where

    describe Nerode_Separation i = vcat
        [ text "gesucht ist ein (möglichst kleiner) regulärer Ausdruck" 
        , text "für eine Sprache L, so daß die Wörter"
        , nest 4 $ vcat $ do
                ( k, w ) <- zip [0 :: Int ..] i
                return $ text "u_" <> toDoc k <+> equals <+> toDoc w
        , text "bezüglich der Nerode-Kongruenz von L paarweise inkongruent sind."
	, text "Für jedes Paar (i,j) mit i < j"
	, text "sollen Sie ein Wort w angeben"
	, text "mit (u_i . w  in  L) <=> (u_j . w  not in  L)"
        ]

    initial Nerode_Separation i = 
        NFA.Nerode.Separation.Solution.example ( take 5 $ concat i )

    partial Nerode_Separation i s = do
        let alpha = mkSet $ concat i
        Exp.Test.tests [ -- Exp.Property.Alphabet alpha
                         Exp.Property.Simple 
                       ] $ language s 


    total Nerode_Separation i s = do
        let alpha = nub $ concat i
        let d = Autolib.Exp.Inter.inter_det
              ( std_sigma alpha )
              ( language s )

        C.partial_check d $ S.Solution { S.words = i , S.proofs = proofs s }

        let n = length i
	C.complete_map n $ proofs s

instance NFAC c Int => C.Measure Nerode_Separation 
                   [ [c] ]
		   ( Solution c )
  where
    measure _ _ s = fromIntegral $ size $ language s

make :: Make
make = direct Nerode_Separation [ "abb", "baba" ]

                                   
