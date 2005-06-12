module Inter.Collector where

import Inter.Types

import qualified Blank
import qualified Upload

import qualified PCProblem.Quiz
import qualified Boolean.Instance
import qualified Boolean.Quiz
import qualified Sortier.Netz.Check 
import qualified JVM.Make
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
import qualified Hanoi.Semantik
import qualified Hanoi.Quiz
import qualified Type.Check
import qualified Type.Quiz

-- import qualified Object.Check
-- import qualified Object.Quiz

import qualified Palindrom.Plain

import qualified Faktor.Faktor

import qualified NFA.Convert
import qualified NFA.Equiv.Challenger
import qualified Exp.Convert
import qualified Exp.Smaller
import qualified Grammatik.CF.Interface
import qualified NPDA.Inter
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

import qualified Number.Base.Central
import qualified Number.Float.From
import qualified Number.Float.To

import qualified LCS.Instance

import qualified Graph.MST.Plain
import qualified Graph.MST.Quiz

import qualified Graph.VC.Central
import qualified Graph.VC.VCSAT
import qualified Partition.Central

import qualified KnapsackFraction.Central

import qualified RM.Make

import qualified Code.Huffman.Boiler
import qualified Code.Quiz
import qualified Code.Class
import qualified Code.Param
import qualified Code.Move_To_Front as MTF
import qualified Code.Burrows_Wheeler as BW
import qualified Code.LZ
import qualified Code.Compress


makers :: [ Make ]
makers = [ Blank.make
	 , Upload.make
	 , PCProblem.Quiz.make_quiz
	 , PCProblem.Quiz.make_fixed
	 , Boolean.Instance.make
	 , Boolean.Quiz.make
	 , Sortier.Netz.Check.make
	 , JVM.Make.make
	 , Graph.Selfcom.make
	 , Graph.Nachbar.make
	 , Graph.Cross.make
	 , Graph.MinSep.make
	 , Robots.Interface.make
	 , Robots.Interface.qmake
	 , Graph.Col.Plain.make
	 , Graph.Col.Quiz.make
	 , Graph.Cage.Central.make
	 , Graph.Graceful.Central.make
	 , Collatz.Plain.make
	 , Collatz.Plain.qmake
	 , Collatz.Inverse.make
	 , Collatz.Inverse.qmake
	 , Hanoi.Semantik.make
	 , Hanoi.Quiz.make
	 , Type.Check.make
	 , Type.Quiz.make
	 , Palindrom.Plain.make

	 , NFA.Convert.make
	 , NFA.Convert.qmake
	 , Exp.Convert.make
	 , Exp.Convert.qmake
	 , Exp.Smaller.make
	 , NFA.Equiv.Challenger.make
	 , NFA.Equiv.Challenger.qmake

         , Grammatik.CF.Interface.make
	 , NPDA.Inter.make

         , SAT.SAT.make_fixed
         , SAT.SAT.make_quiz

         , Baum.Reconstruct.make_fixed
         , Baum.Reconstruct.make_quiz
         , Baum.Binary.make_quiz
         , Baum.ZweiDrei.make_quiz

         , Graph.TreeWidth.make
         , Graph.PartialKTree.make
         , Graph.PartialKTree.qmake

         , Faktor.Faktor.make_fixed
         , Faktor.Faktor.make_quiz

	 , Graph.Bi.Quiz.make
	 , Graph.Bi.Plain.make

	 , Graph.Circle.Quiz.make
	 , Graph.Circle.Plain.make

	 , Graph.Bisekt.Plain.make
	 , Graph.Bisekt.Quiz.make

         , Number.Base.Central.make_fixed
         , Number.Base.Central.make_quiz

         , Number.Float.From.make_fixed
         , Number.Float.From.make_quiz
         , Number.Float.To.make_fixed
         , Number.Float.To.make_quiz

         , LCS.Instance.make_fixed
         , LCS.Instance.make_quiz

	 , Graph.Way.Plain.make
	 , Graph.Way.Quiz.make

	 , Graph.MST.Plain.make
	 , Graph.MST.Quiz.make

         , Partition.Central.make_fixed
         , Partition.Central.make_quiz

         , KnapsackFraction.Central.make_fixed
         , KnapsackFraction.Central.make_quiz

         , Graph.VC.Central.make_fixed
         , Graph.VC.Central.make_quiz
         , Graph.VC.VCSAT.make_fixed
         , Graph.VC.VCSAT.make_quiz

         , RM.Make.make

         , Code.Huffman.Boiler.make_fixed
         , Code.Huffman.Boiler.make_quiz

         , Code.Class.enc BW.Burrows_Wheeler
         , Code.Class.dec BW.Burrows_Wheeler ( "abracadabra", 2 )

         , Code.Quiz.enc MTF.Move_To_Front
         , Code.Quiz.enc BW.Burrows_Wheeler
         , Code.Quiz.dec MTF.Move_To_Front
         , Code.Quiz.dec BW.Burrows_Wheeler

         , Code.Compress.make_quiz Code.LZ.Lempel_Ziv_Welch
         , Code.Compress.make_quiz Code.LZ.Lempel_Ziv_77
	 ]
