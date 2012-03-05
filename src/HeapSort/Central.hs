{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections, FlexibleInstances,  NoMonomorphismRestriction #-}

module HeapSort.Central where

import HeapSort.Data
import HeapSort.Operation
import HeapSort.Tree as T
import HeapSort.GraphViz (toPng)
import HeapSort.Semantics
import Tree.Class (ToTree(..))

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, (<>), hsep, toDoc, nest, ToDoc(..), docParen, fsep, (</>))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (Reporter, reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Inter.Types (OrderScore(..), ScoringOrder(Increasing), direct)

import Data.Typeable (Typeable)
import Control.Monad (when,unless)
import Control.Monad.State
import Data.Derive.All (makeEq)
import qualified Data.Tree

rejectIO = Autolib.Reporter.IO.Type.reject
informIO = Autolib.Reporter.IO.Type.inform

data HeapSort = HeapSort deriving Typeable

$(derives [makeReader, makeToDoc] [''HeapSort])

make_fixed = direct HeapSort $ Config OnFailure [5,4,2,1,3,6]

instance OrderScore HeapSort where
    scoringOrder h = Increasing

instance Verify HeapSort Config where
    verify _ cfg = do
        return ()

$(derives [makeEq, makeToDoc] [''Tree])

data Verbose   a = VerboseReporter { runVerbose :: StateT (Maybe Operation) Reporter a }
data OnFailure a = OnFailureReporter { runOnFailure :: StateT (Tree (Marked Int)) (StateT (Maybe Operation) Reporter) a }

instance Monad Verbose where
    return = VerboseReporter . return
    (VerboseReporter mx) >>= f = VerboseReporter $ mx >>= runVerbose . f
    fail x = VerboseReporter $ do
        mb_op <- get
        lift $ reject $ text $ "Nein. " ++ case mb_op of {Nothing -> ""; Just op -> "Operation '" ++ show op ++ "' ist nicht möglich. "} ++ x

instance Monad OnFailure where
    return = OnFailureReporter . return
    (OnFailureReporter mx) >>= f = OnFailureReporter $ mx >>= runOnFailure . f
    fail x = OnFailureReporter $ do
        t  <- get
        lift $ lift $ inform $ text $ toPng $ toTree t
        mb_op <- lift get
        lift $ lift $ reject $ text $ "Nein. " ++ case mb_op of {Nothing -> ""; Just op -> "Operation '" ++ show op ++ "' ist nicht möglich. "} ++ x

instance TreeOutputMonad (Marked Int) Verbose where
    treeOutput x = VerboseReporter $ lift $ inform $ text $ toPng $ toTree x
instance TreeOutputMonad (Marked Int) OnFailure where
    treeOutput x = OnFailureReporter $ put x
instance OperationOutputMonad Verbose where
    operationOutput x = VerboseReporter $ put $ Just x
instance OperationOutputMonad OnFailure where
    operationOutput x = OnFailureReporter $ lift $ put $ Just x


instance Partial HeapSort Config Solution where
    report p (Config feedback numbers) = do
      inform $ vcat [ text "Führen Sie den Heap-Sort-Algorithmus auf folgendem Binärbaum durch:"
                    , text ""
                    , text $ toPng $ toTree $ T.fromList numbers
                    , text ""
                    , text "Als Operationen stehen ihnen S (Sinken) und T (Tauschen) zur Verfügung."
                    , text ""
                    , text "Also zum Beispiel (für einen entsprechenden Baum):"
                    , text ""
                    , nest 3 $ vcat [ text "Solution"
                                    , text "  [ S(55) [L,R]"
                                    , text "  , S(31) [R]"
                                    , text "  , T(23,66)"
                                    , text "  ]"
                                    ]
                    , text ""
                    , text "Die erste Operation lässt Knoten 55 erst nach links, dann (im selben Zug) nach rechts absinken."
                    , text "Die zweite Operation senkt Knoten 31 nach rechts ab. Die dritte Operation vertauscht Knoten 23"
                    , text "und 66 (und somit wird 66 ans Ende des Arrays bewegt und als abgespalten markiert)."
                    ]
      when (feedback /= None) $ do
        inform $ vcat [ text ""
                      , text "Zum Ausgabeformat (nicht Teil Ihrer Eingabe):"
                      , hsep [ case feedback of { OnFailure -> text "Bei Fehlern"; Verbose -> text "Nach jeder Operation" }
                             , text "wird der jeweils aktuelle Baum ausgegeben."
                             ]
                      , text "Bereits als abgespalten markierte Knoten (nach Verwendung von Tauschen) werden im Baum mit eckigen Klammern dargestellt."
                      ]

      inform $ vcat [ text ""
                    , text "Alle Knoten bis auf die Wurzel müssen am Ende markiert sein, damit der Algorithmus als vollständig durchgeführt gilt."
                    ]

      when (feedback == None) $ do
        inform $ vcat [ text ""
                      , text "Hinweis: Bei dieser Aufgabe wird keine Rückmeldung über Korrektheit der Lösung gegeben."
                      , text "         Wenn eine Einsendung akzeptiert wird, heißt dies nicht, dass sie korrekt ist."
                      ]

    initial p _ = Solution []

    total _ (Config None _) _ = do
        inform $ vcat [ text "Nicht geprüft."
                      , text ""
                      , text "Die Einsendung wird von Ihrem Tutor bewertet."
                      , text ""
                      , text "Ignorieren Sie die unten angezeigte Bewertung."
                      ]
    total p (Config feedback {- OnFailure or Verbose -} unsortedNumbers) (Solution operations) = do
        let t = decorate (T.fromList unsortedNumbers)
        let m = execute_ operations t
        t' <- case feedback of
                 OnFailure ->
                      flip evalStateT Nothing $ flip evalStateT t $ runOnFailure m
                 Verbose ->
                      flip evalStateT Nothing $ runVerbose m
        unless (isSorted $ map value $ T.toList t') $ do
            when (feedback == OnFailure) $ do
               inform $ text $ toPng $ toTree t'
            reject $ text "Nein. Baum entspricht nicht einer sortierten Liste."
        unless (all isMarked $ tail $ T.toList t') $ do
            when (feedback == OnFailure) $ do
               inform $ text $ toPng $ toTree t'
            reject $ text "Nein. Es sind nicht alle Knoten markiert. Der Algorithmus würde hier noch nicht terminieren, obwohl die Elemente sortiert sind."
        inform $ vcat [ text "Ja, Ihre Einsendung ist richtig."
                      , text ""
                      , text "Ignorieren Sie die unten angezeigte Bewertung."
                      ]

value (Marked x)   = x
value (Unmarked x) = x

isMarked (Marked _)   = True
isMarked (Unmarked _) = False

isSorted []         = True
isSorted [x]        = True
isSorted (x1:x2:xs) = x1 <= x2 && isSorted (x2:xs)

instance ToTree (Data.Tree.Tree String) where
  toTree = id

instance ToTree (T.Tree Int) where
  toTree = Data.Tree.unfoldTree uf
    where
      uf (T.Branch x Empty Empty) = (show x,[])
      uf (T.Branch x l     Empty) = (show x,[l])
      uf (T.Branch x Empty r    ) = (show x,[r])
      uf (T.Branch x l     r    ) = (show x,[l,r])

instance ToTree (T.Tree (Marked Int)) where
  toTree = Data.Tree.unfoldTree uf
    where
      uf (T.Branch x Empty Empty) = (showMarked x,[])
      uf (T.Branch x l     Empty) = (showMarked x,[l])
      uf (T.Branch x Empty r    ) = (showMarked x,[r])
      uf (T.Branch x l     r    ) = (showMarked x,[l,r])

      showMarked (Marked   x) = "[" ++ show x ++ "]"
      showMarked (Unmarked x) = show x
