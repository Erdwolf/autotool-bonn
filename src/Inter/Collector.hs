module Inter.Collector where

import Inter.Types
import Data.Tree

import qualified Blank
import qualified Upload

import qualified PCProblem.Quiz
import qualified Boolean.Instance
import qualified Boolean.Quiz
import qualified Boolean2.Instance
import qualified Boolean2.Quiz
import qualified Sortier.Netz.Check
import qualified Sortier.Merge.Check
import qualified Sortier.Median.Check
import qualified Sortier.Programm.Check

import qualified JVM.Make
import qualified Turing.Make
import qualified Brainfuck.Make
import qualified Peano.Make

import qualified Fun.Quiz
import qualified Fun.Make


import qualified Fun.Direct

import qualified Haskell.Central
import qualified Haskell.Blueprint.Central
import qualified Haskell.Syntax.Central

import qualified Prolog.Programming.Central

import qualified Prolog.Unifier.Central

import qualified Syntax.Central
import qualified Syntax.Quiz

import qualified Specify.Inter

import qualified String_Matching.KMP.Central
import qualified String_Matching.BM.Central

import qualified Graph.Selfcom
import qualified Graph.Nachbar
import qualified Graph.Cross
import qualified Graph.MinSep
import qualified Graph.Ramsey
import qualified Graph.Van_der_Waerden
import qualified Graph.Col.Greedy.Central

import qualified Graph.MST.Plain
import qualified Graph.MST.Quiz

import qualified Graph.TSP.Plain
import qualified Graph.TSP.Quiz

import qualified Robots.Interface
import qualified Robots.Inverse

import qualified Robots3.Interface
import qualified Robots3.Inverse


import qualified Rushhour.Central

import qualified Graph.Col.Plain
import qualified Graph.Col.Quiz
import qualified Graph.Col.Gadget.Central

import qualified Graph.Cage.Central
import qualified Graph.Graceful.Central
import qualified Collatz.Plain
import qualified Collatz.Inverse
import qualified Collatz.Long
import qualified Hanoi.Semantik
import qualified Hanoi.Quiz
import qualified Type.Check
import qualified Type.Quiz
import qualified Type.Poly.Check
import qualified Type.Poly.Quiz
import qualified FP.Check
import qualified FP.Quiz

import qualified Palindrom.Plain

import qualified Faktor.Faktor
import qualified Faktor.Times
import qualified Faktor.Euklid
import qualified Faktor.Inverse

import qualified RSA.Break
import qualified RSA.Quiz

import qualified Diffie_Hellman.Break
import qualified Diffie_Hellman.Quiz

import qualified NFA.Convert
import qualified NFA.Equiv.Challenger
import qualified NFA.Nerode.Congruent.Check
import qualified NFA.Nerode.Congruent.Quiz
import qualified NFA.Nerode.Incongruent.Check
import qualified NFA.Nerode.Incongruent.Quiz
import qualified NFA.Nerode.Separation.Check
import qualified NFA.Compress.Inter
import qualified Exp.Convert
import qualified Exp.Smaller
import qualified Exp.Shortest_Missing

import qualified Pump.Inter
import qualified Pump.Inter2

import qualified Grammatik.CF.Interface
import qualified Grammatik.CF.Interface2
import qualified NPDA.Inter
import qualified NPDA.Inter2

import qualified Grammatik.Akzeptor

import qualified CNF.Optimize
import qualified SAT.SAT
import qualified Baum.Reconstruct
import qualified Baum.Binary
import qualified Baum.ZweiDrei
import qualified Baum.AVL
import qualified Baum.RedBlack

import qualified Baum.List
import qualified Baum.BinHeap
import qualified Baum.Leftist
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

import qualified Number.Base.Central
import qualified Number.Float.From
import qualified Number.Float.To

import qualified LCS.Instance
import qualified SCS.Instance

import qualified Graph.VC.Central
import qualified Graph.VC.VCSAT
import qualified Partition.Central

import qualified Binpack.Interface
import qualified Binpack.FFD
import qualified KnapsackFraction.Central

import qualified RM.Make
import qualified RAM.Make

import qualified Program.General.Central
import qualified Program.GeneralBonn.Central
import qualified Program.Array.Instance
import qualified Program.ArrayBonn.Instance
import qualified Program.List.Instance

import qualified Program.Cexp.Interface

import qualified Code.Huffman.Boiler
import qualified Code.Quiz
import qualified Code.Class
import qualified Code.Param
import qualified Code.Move_To_Front as MTF
import qualified Code.Burrows_Wheeler as BW
import qualified Code.LZ
import qualified Code.Compress
import qualified Code.Hamming
import qualified Code.Nonprefix 
import qualified Code.Nocode

import qualified Rewriting.TRS.Derive as RD
import qualified Rewriting.TRS.Numerical

import qualified Rewriting.Derive
import qualified Rewriting.TRS.Apply
import qualified Rewriting.SRS.Apply
import qualified Lambda.Apply

import qualified Lambda.Derive
import qualified Lambda.Backward_Join

import qualified CL.Find

import qualified Unify.Main

import qualified PL.Find_Model
import qualified PL.Split.Term.Fixed
import qualified PL.Split.Term.Quiz
import qualified Hilbert.Central
import qualified Resolution.Central

import qualified Algebraic.Central
import qualified Algebraic.Quiz

import Algebraic.Graph
import Algebraic.STGraph

import qualified Algebraic2.Central
import qualified Algebraic2.Quiz

import Algebraic.Integer
import Algebraic.Set
import Algebraic.Relation

import qualified Flow.Central

import qualified Petri.Reach
import qualified Petri.Deadlock
import qualified Petri.Remote

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
	       , item NFA.Compress.Inter.make_fixed
	       , item NFA.Compress.Inter.make_quiz
          ]
       , heading "reguläre Ausdrücke"
	       [ item Exp.Convert.make
	       , item Exp.Convert.qmake
	       , item Exp.Smaller.make
	       , item Exp.Shortest_Missing.make
          ]
       , heading "Grammatiken (neu)"
          [ item Grammatik.CF.Interface2.make
          , item Grammatik.Akzeptor.acceptor
          ]
       , heading "Grammatiken (veraltet)"
          [ item Grammatik.CF.Interface.make
	       ]
       , heading "Kellerautomaten (neu)"
	       [ item NPDA.Inter2.make
          ]
       , heading "Kellerautomaten (veraltet)"
	       [ item NPDA.Inter.make
          ]
       , heading "Pumping-Lemma (neu)"
          [ item Pump.Inter2.reg
          , item Pump.Inter2.cf
          ]
       , heading "Pumping-Lemma (veraltet)"
          [ item Pump.Inter.reg
          , item Pump.Inter.cf
          ]
       , heading "Nerode-Kongruenz"
          [ item NFA.Nerode.Congruent.Check.make
          , item NFA.Nerode.Congruent.Quiz.make
          , item NFA.Nerode.Incongruent.Check.make
          , item NFA.Nerode.Incongruent.Quiz.make
          , item NFA.Nerode.Separation.Check.make
          ]
       , heading "Turing-Maschine (als Akzeptor)"
          [ item Turing.Make.acceptor
          ]
       , heading "Syntaxdiagramme"
          [ item Syntax.Central.make_fixed
          , item Syntax.Quiz.make
          ]
       ]
    , heading "Logik"
	    [ heading "Aussagenlogik"
          [ heading "Umformen (neu)"
             [ item Boolean2.Instance.make
	          , item Boolean2.Quiz.make
             ]
          , heading "Umformen (alt)"
             [ item Boolean.Instance.make
	          , item Boolean.Quiz.make
             ]
	       , item SAT.SAT.make_fixed
	       , item SAT.SAT.make_quiz
          , item CNF.Optimize.make_fixed
	       , item Hilbert.Central.make_fixed
	       , item Resolution.Central.make_fixed
	       , item Resolution.Central.make_quiz
	       ]
	    , heading "Prädikatenlogik"
	       [ item PL.Find_Model.make_fixed
          , item PL.Split.Term.Fixed.make
          , item PL.Split.Term.Quiz.make
	       ]
	    ]
    , heading "Mengen und Relationen"
       [ item $ Algebraic2.Central.make Algebraic_Set
	    , item $ Algebraic2.Quiz.make Algebraic_Set
	    , item $ Algebraic2.Central.make Algebraic_Relation
	    , item $ Algebraic2.Quiz.make Algebraic_Relation
       ]
    , heading "Kombinatorik"
       [ item PCProblem.Quiz.make_quiz
	    , item PCProblem.Quiz.make_fixed
       , heading "Ramsey-Theorie"
          [ item Graph.Ramsey.make
          , item Graph.Van_der_Waerden.make
          ]
	    , heading "Lunar Lockout (original)"
	 	   [ item Robots.Interface.make
	 	   , item Robots.Interface.qmake
	 	   , item Robots.Inverse.make
	 	   , item Robots.Inverse.qmake
	 	   ]
       , heading "Solar Lockout (modified)"
	 	   [ item Robots3.Interface.make
	 	   , item Robots3.Interface.qmake
	 	   , item Robots3.Inverse.make
	 	   , item Robots3.Inverse.qmake
	 	   , item Rushhour.Central.make
	 	   ]
--      , item Rushhour.Central.qmake
	   , item Hanoi.Semantik.make
	   , item Hanoi.Quiz.make
      , heading "lange gemeinsame Teilfolge"
         [ item LCS.Instance.make_fixed
         , item LCS.Instance.make_quiz
         ]
      , heading "kurze gemeinsame Superfolge"
         [ item SCS.Instance.make_fixed
         , item SCS.Instance.make_quiz
         ]
      ]
   , heading "Berechnungsmodelle"
      [ item JVM.Make.make
      , item Turing.Make.computer
      , item Program.Cexp.Interface.make_fixed
      , heading "Programme mit Arrays"
         [ item $ Program.General.Central.make_fixed Program.Array.Instance.Program_Array
         , item Program.Array.Instance.make_quiz
         ]
      , heading "Programme mit Arrays (Bonn)"
         [ item $ Program.GeneralBonn.Central.make_fixed Program.ArrayBonn.Instance.Program_Array
         , item Program.ArrayBonn.Instance.make_quiz
         ]
      , heading "Programme mit Listen, Stacks, Queues"
         [ heading "Programme mit Listen"
            [ item $ Program.General.Central.make_fixed Program.List.Instance.Program_List
            , item Program.List.Instance.make_quiz
            ]
         ]
      , heading "Primitiv rekursive Funktionen (neu)"
         [ item Fun.Direct.make_fixed
         , item Fun.Direct.make_quiz
         ]
      , heading "Primitiv rekursive Funktionen (alt)"
         [ item Fun.Make.make
         , item Fun.Quiz.make
         ]
      , item RAM.Make.make
      , item RM.Make.make
      , item Brainfuck.Make.computer
      , heading "Peano-Zahlen und Folds"
         [ item Peano.Make.make
         ]
      ]
   , heading "Petri-Netze"
      [ item Petri.Reach.make_fixed
      , item Petri.Reach.make_quiz
      , item Petri.Deadlock.make_fixed
      , item Petri.Deadlock.make_quiz
      , heading "Philosophen"
         [ item $ Petri.Deadlock.make_diner 4
         ]
      , item Petri.Remote.make_fixed
      ]
   , heading "Terme, Ersetzungssysteme"
      [ heading "Wortersetzung"
         [ item $ Rewriting.Derive.make_fixed Rewriting.SRS.Apply.For_SRS
         ]
      , heading "Termersetzung"
         [ item $ Rewriting.Derive.make_fixed Rewriting.TRS.Apply.For_TRS
         , item RD.make_quiz
         , item Rewriting.TRS.Numerical.make
         ]
      , heading "Lambda-Kalkül"
         [ item $ Rewriting.Derive.make_fixed Lambda.Apply.For_Lambda
         ]
      , heading "Kombinatorische Logik"
         [ item CL.Find.make_fixed
         ]
      , heading "Unifikation"
         [ item Unify.Main.make_fixed
         , item Unify.Main.make_quiz
         ]
      ]
   , heading "Lambda-Kalkül"
      [ item Lambda.Derive.make_fixed
      , item Lambda.Derive.make_quiz
      , item Lambda.Backward_Join.make_fixed
      ]
   , heading "Kombinatorische Logik"
      [ item $ CL.Find.make_fixed
      ]
   , heading "Graphen"
      [ item Graph.Selfcom.make
--      , item Graph.Nachbar.make
      , item Graph.Cross.make
      , item Graph.Ramsey.make
      , item Graph.MinSep.make
      , item Graph.Col.Plain.make
      , item Graph.Col.Quiz.make
      , item Graph.Col.Greedy.Central.make
--      , item Graph.Cage.Central.make
      , item Graph.Col.Gadget.Central.make
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
      , item Graph.MST.Plain.make
      , item Graph.MST.Quiz.make
      , item Graph.TSP.Plain.make
      , item Graph.TSP.Quiz.make
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
      , item Type.Poly.Check.make
      , item Type.Poly.Quiz.make
      , item FP.Check.make
      , item FP.Quiz.make
      , item Flow.Central.goto_to_struct_fixed
      , item Flow.Central.struct_to_goto_fixed
      , item Specify.Inter.make
      , heading "Haskell-Programmierung"
         [ item Haskell.Blueprint.Central.make_fixed
         , item Haskell.Syntax.Central.make_fixed
         , item Haskell.Central.make_fixed
         ]
      , heading "Prolog-Programmierung"
         [ item Prolog.Programming.Central.make_fixed
         , item Prolog.Unifier.Central.make_fixed
         ]
      ]
   , heading "Algorithmen"
      [ heading "Sortiernetze"
         [ item Sortier.Netz.Check.make
         , item Sortier.Merge.Check.make
         , item Sortier.Median.Check.make
         ]
      , heading "Sortierprogramme"
         [ item Sortier.Programm.Check.make
         ]
      , heading "String-Matching"
         [ item String_Matching.KMP.Central.make_fixed
         , item String_Matching.KMP.Central.make_quiz
         , item String_Matching.BM.Central.make_fixed
         , item String_Matching.BM.Central.make_quiz
         ]
      ]
   , heading "Datenstrukturen"
      [ heading "Bäume"
         [ heading "Pre/In/Post/Level-Order"
            [ item Baum.Reconstruct.make_fixed
            , item Baum.Reconstruct.make_quiz
            ]
         , heading "Suchbäume"
            [ item Baum.Binary.make_quiz
            , item Baum.AVL.make_quiz
            , item Baum.RedBlack.make_quiz
            , item Baum.ZweiDrei.make_quiz
            ]
         , heading "Heap-geordnete Bäume"
            [ item Baum.List.make_fixed
            , item Baum.List.make_quiz
            , item Baum.BinHeap.make_fixed
            , item Baum.BinHeap.make_quiz
            , item Baum.Leftist.make_fixed
            , item Baum.Leftist.make_quiz
            ]
         ]
      , heading "Programme mit Arrays"
         [ item $ Program.General.Central.make_fixed Program.Array.Instance.Program_Array
         , item Program.Array.Instance.make_quiz
         ]
      , heading "Programme mit Listen"
         [ item $ Program.General.Central.make_fixed Program.List.Instance.Program_List
         , item Program.List.Instance.make_quiz
         ]
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
   , heading "Codierung, Kompression, Verschlüsselung"
      [ heading "Codierung"
         [ item Code.Nonprefix.make_fixed
         , item Code.Nocode.make_fixed
         , item Code.Nocode.make_quiz
         , item Code.Huffman.Boiler.make_fixed
         , item Code.Huffman.Boiler.make_quiz
         , item $ Code.Class.enc BW.Burrows_Wheeler
         , item $ Code.Class.dec BW.Burrows_Wheeler ("abracadabra", 2)
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
      , heading "Verschlüsselung"
         [ item $ RSA.Break.make
         , item $ RSA.Quiz.make
         , item $ Diffie_Hellman.Break.make
         , item $ Diffie_Hellman.Quiz.make
         ]
      ]
   , heading "NP-vollständige Probleme"
      [ item Binpack.Interface.make_fixed
      , item Binpack.Interface.make_quiz
      , item Binpack.FFD.make_fixed
      , item Partition.Central.make_fixed
      , item SAT.SAT.make_fixed
      , item SAT.SAT.make_quiz
      , item Partition.Central.make_quiz
      , item KnapsackFraction.Central.make_fixed
      , item KnapsackFraction.Central.make_quiz
      , item Graph.VC.Central.make_fixed
      , item Graph.VC.Central.make_quiz
      , item Graph.VC.VCSAT.make_fixed
      , item Graph.VC.VCSAT.make_quiz
      ]
   , heading "experimentell"
      [ item $ Algebraic2.Central.make Algebraic_Integer
      , item $ Algebraic2.Quiz.make Algebraic_Integer
      , item $ Algebraic.Central.make Algebraic_Graph
      , item $ Algebraic.Quiz.make Algebraic_Graph
      , item $ Algebraic.Central.make Algebraic_STGraph
      , item $ Algebraic.Quiz.make Algebraic_STGraph
      ]
   , heading "noch nicht eingeordnet"
      [ item Palindrom.Plain.make
      ]
   ]
