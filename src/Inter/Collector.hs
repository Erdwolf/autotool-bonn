{-# OPTIONS -fallow-overlapping-instances #-}

module Inter.Collector where

import Inter.Types
import Data.Tree

import qualified Blank
import qualified Upload

import qualified PCProblem.Quiz
import qualified Boolean.Instance
import qualified Boolean.Quiz
import qualified Sortier.Netz.Check 
import qualified Sortier.Merge.Check 
import qualified Sortier.Median.Check 

import qualified JVM.Make
import qualified Turing.Make
import qualified Fun.Quiz
import qualified Fun.Make

import qualified Graph.Selfcom
import qualified Graph.Nachbar
import qualified Graph.Cross
import qualified Graph.MinSep
import qualified Robots.Interface
import qualified Graph.Col.Plain
import qualified Graph.Col.Quiz

import qualified Graph.Cage.Central
import qualified Graph.Graceful.Central
import qualified Collatz.Plain
import qualified Collatz.Inverse
import qualified Collatz.Long
import qualified Hanoi.Semantik
import qualified Hanoi.Quiz
import qualified Type.Check
import qualified Type.Quiz

-- import qualified Object.Check
-- import qualified Object.Quiz

import qualified Palindrom.Plain

import qualified Faktor.Faktor
import qualified Faktor.Times
import qualified Faktor.Euklid
import qualified Faktor.Inverse

import qualified NFA.Convert
import qualified NFA.Equiv.Challenger
import qualified Exp.Convert
import qualified Exp.Smaller

import qualified Pump.Inter

-- import qualified Grammatik.Interface
import qualified Grammatik.CF.Interface
import qualified NPDA.Inter
-- import qualified Turing.Inter

import qualified SAT.SAT
import qualified Baum.Reconstruct
import qualified Baum.Binary
import qualified Baum.ZweiDrei
import qualified Graph.TreeWidth
import qualified Graph.PartialKTree

import qualified Graph.Bi.Quiz
import qualified Graph.Bi.Plain
import qualified Graph.Circle.Quiz
import qualified Graph.Circle.Plain

import qualified Graph.Bisekt.Plain
import qualified Graph.Bisekt.Quiz

import qualified Graph.Way.Plain
import qualified Graph.Way.Quiz

import qualified Graph.Hamilton.Plain
import qualified Graph.Hamilton.Quiz

-- broken with 6.6
-- import qualified Graph.MST.Plain
-- import qualified Graph.MST.Quiz

import qualified Number.Base.Central
import qualified Number.Float.From
import qualified Number.Float.To

import qualified LCS.Instance

import qualified Graph.VC.Central
import qualified Graph.VC.VCSAT
import qualified Partition.Central

import qualified KnapsackFraction.Central

import qualified RM.Make
import qualified RAM.Make

import qualified Code.Huffman.Boiler
import qualified Code.Quiz
import qualified Code.Class
import qualified Code.Param
import qualified Code.Move_To_Front as MTF
import qualified Code.Burrows_Wheeler as BW
import qualified Code.LZ
import qualified Code.Compress
import qualified Code.Hamming

import qualified Rewriting.Derive
import qualified Rewriting.Numerical

import qualified Lambda.Derive
import qualified Lambda.Backward_Join

import qualified PL.Find_Model
import qualified Hilbert.Central

import qualified Algebraic.Central
import qualified Algebraic.Quiz
import Algebraic.Integer
import Algebraic.Graph
import Algebraic.STGraph

import qualified Flow.Central

makers :: [ Make ]
makers = do Right make <- flatten tmakers ; return make

heading :: h -> [ Tree ( Either h i ) ] -> Tree ( Either h i )
heading h ts = Node ( Left h ) ts

item :: i -> Tree ( Either h i )
item i = Node ( Right i ) []

tmakers :: Tree ( Either String Make )
tmakers = 
    heading "Aufgaben" 
         [ item Blank.make
	 , item Upload.make
         , heading "Automaten und Formale Sprachen" 
                [ heading "endliche Automaten"
                     [ item NFA.Convert.make
	             , item NFA.Convert.qmake
	             , item NFA.Equiv.Challenger.make
	             , item NFA.Equiv.Challenger.qmake
                     ]
                , heading "reguläre Ausdrücke"
	             [ item Exp.Convert.make
	             , item Exp.Convert.qmake
	             , item Exp.Smaller.make
                     ]
                , heading "Grammatiken"
                     [ item Grammatik.CF.Interface.make
                     -- , item Grammatik.Interface.make
                     ]
                , heading "Kellerautomaten"
	             [ item NPDA.Inter.make
                     ]
                , heading "Pumping-Lemma"
                     [ item Pump.Inter.reg
                     , item Pump.Inter.cf
                     ]
                , heading "Turing-Maschine (als Akzeptor)"
                     [ item Turing.Make.acceptor
                     ]
                ]
         , heading "Logik"
	        [ heading "Aussagenlogik"
                    [ item Boolean.Instance.make
		    , item Boolean.Quiz.make
		    , item SAT.SAT.make_fixed
		    , item SAT.SAT.make_quiz
		    , item Hilbert.Central.make_fixed
		    ]
		, heading "Prädikatenlogik"
		    [ item PL.Find_Model.make_fixed
		    ]
		]
         , heading "Kombinatorik"
                [ item PCProblem.Quiz.make_quiz 
	        , item PCProblem.Quiz.make_fixed
	        , item Robots.Interface.make
	        , item Robots.Interface.qmake
	        , item Hanoi.Semantik.make
	        , item Hanoi.Quiz.make
                , item LCS.Instance.make_fixed
                , item LCS.Instance.make_quiz
                ]
         , heading "Berechnungsmodelle"
                [ item JVM.Make.make
                , item Turing.Make.computer
		, item Fun.Make.make
		, item Fun.Quiz.make
                , item RAM.Make.make
                , item RM.Make.make
                ]
         , heading "Termersetzung"
                [ item Rewriting.Derive.make_fixed
                , item Rewriting.Derive.make_quiz
                , item Rewriting.Numerical.make
                ]
         , heading "Lambda-Kalkül"
                [ item Lambda.Derive.make_fixed
                , item Lambda.Derive.make_quiz
                , item Lambda.Backward_Join.make_fixed
                ]
         , heading "Graphen"
                [ item Graph.Selfcom.make
                , item Graph.Nachbar.make
	        , item Graph.Cross.make
	        , item Graph.MinSep.make
	        , item Graph.Col.Plain.make
	        , item Graph.Col.Quiz.make
	        , item Graph.Cage.Central.make
	        , item Graph.Graceful.Central.make
                , item Graph.TreeWidth.make
                , item Graph.PartialKTree.make
                , item Graph.PartialKTree.qmake
	        , item Graph.Bi.Quiz.make
	        , item Graph.Bi.Plain.make
	        , item Graph.Circle.Quiz.make
	        , item Graph.Circle.Plain.make
	        , item Graph.Bisekt.Plain.make
	        , item Graph.Bisekt.Quiz.make
                , item Graph.Way.Plain.make
	        , item Graph.Way.Quiz.make
--	        , item Graph.MST.Plain.make
--	        , item Graph.MST.Quiz.make
                , item Graph.Hamilton.Plain.make
                , item Graph.Hamilton.Quiz.make
		, heading "Graphoperationen"
		  [ item $ Algebraic.Central.make Algebraic_Graph
		  , item $ Algebraic.Quiz.make Algebraic_Graph
		  , item $ Algebraic.Central.make Algebraic_STGraph
		  , item $ Algebraic.Quiz.make Algebraic_STGraph
		  ]
                ]
         , heading "Programmierung"
                [ item Type.Check.make
	        , item Type.Quiz.make
		, item Flow.Central.goto_to_struct_fixed
		, item Flow.Central.struct_to_goto_fixed
                ]
         , heading "Algorithmen"
                [ heading "Sortiernetze"
		  [ item Sortier.Netz.Check.make
		  , item Sortier.Merge.Check.make
		  , item Sortier.Median.Check.make
		  ] 
                ]
         , heading "Datenstrukturen"
                [ item Baum.Reconstruct.make_fixed
                , item Baum.Reconstruct.make_quiz
                , item Baum.Binary.make_quiz
                , item Baum.ZweiDrei.make_quiz
                ]
         , heading "Zahlensysteme"
                [ item Number.Base.Central.make_fixed
                , item Number.Base.Central.make_quiz
                , item Number.Float.From.make_fixed
                , item Number.Float.From.make_quiz
                , item Number.Float.To.make_fixed
                , item Number.Float.To.make_quiz
                ]
         , heading "Zahlentheorie"
                [ item Collatz.Plain.make
	        , item Collatz.Plain.qmake
	        , item Collatz.Inverse.make
	        , item Collatz.Inverse.qmake
	        , item Collatz.Long.make
                , item Faktor.Times.make_fixed
                , item Faktor.Times.make_quiz
                , item Faktor.Faktor.make_fixed
                , item Faktor.Faktor.make_quiz
                , item Faktor.Euklid.make_fixed
                , item Faktor.Euklid.make_quiz
                , item Faktor.Inverse.make_fixed
                , item Faktor.Inverse.make_quiz
                ]
         , heading "Codierung und Kompression"
              [ heading "Codierung"
                    [ item Code.Huffman.Boiler.make_fixed
                    , item Code.Huffman.Boiler.make_quiz
                    , item $ Code.Class.enc BW.Burrows_Wheeler
                    , item $ Code.Class.dec BW.Burrows_Wheeler 
                               ( "abracadabra",  2 )
                    , item $ Code.Quiz.enc MTF.Move_To_Front
                    , item $ Code.Quiz.enc BW.Burrows_Wheeler
                    , item $ Code.Quiz.dec MTF.Move_To_Front
                    , item $ Code.Quiz.dec BW.Burrows_Wheeler
                    , item Code.Hamming.make
                    ]
              , heading "Kompression"
                    [ item $ Code.Compress.make_quiz Code.LZ.Lempel_Ziv_Welch
                    , item $ Code.Compress.make_quiz Code.LZ.Lempel_Ziv_77
                    , item $ Code.Compress.make_fixed Code.LZ.Lempel_Ziv_77
                    ]
              ]
         , heading "noch nicht eingeordnet"
                [ item Palindrom.Plain.make
                , item Partition.Central.make_fixed
                , item Partition.Central.make_quiz
                , item KnapsackFraction.Central.make_fixed
                , item KnapsackFraction.Central.make_quiz
                , item Graph.VC.Central.make_fixed
                , item Graph.VC.Central.make_quiz
                , item Graph.VC.VCSAT.make_fixed
                , item Graph.VC.VCSAT.make_quiz               
                ]
	  , heading "experimentell"
	        [ item $ Algebraic.Central.make Algebraic_Integer
	        , item $ Algebraic.Quiz.make Algebraic_Integer
	        , item $ Algebraic.Central.make Algebraic_Graph
	        , item $ Algebraic.Quiz.make Algebraic_Graph
	        , item $ Algebraic.Central.make Algebraic_STGraph
	        , item $ Algebraic.Quiz.make Algebraic_STGraph
		]
          ]


