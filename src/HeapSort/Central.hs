{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections, FlexibleInstances,  NoMonomorphismRestriction #-}
module HeapSort.Central where

import qualified TextConfig

import HeapSort.Data
import HeapSort.Operation
import HeapSort.Tree as T
import HeapSort.GraphViz (toPng)
import HeapSort.Semantics

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, (<>), hsep, toDoc, nest, ToDoc(..), docParen, fsep, (</>))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (Reporter, reject, inform, output)
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

data Verbose   a = VerboseReporter { runVerbose :: StateT [Operation] Reporter a }
data OnFailure a = OnFailureReporter { runOnFailure :: StateT (Tree (Marked Int)) (StateT [Operation] Reporter) a }

instance Monad Verbose where
    return = VerboseReporter . return
    (VerboseReporter mx) >>= f = VerboseReporter $ mx >>= runVerbose . f
    fail reason = VerboseReporter $ do
        ops <- get
        lift $ rejectOps ops reason

instance Monad OnFailure where
    return = OnFailureReporter . return
    (OnFailureReporter mx) >>= f = OnFailureReporter $ mx >>= runOnFailure . f
    fail reason = OnFailureReporter $ do
        t  <- get
        lift $ lift $ output $ toPng t
        ops <- lift get
        lift $ lift $ rejectOps ops reason


rejectOps []        reason =
    reject $ text $ "Nein. " ++ reason
rejectOps (op:done) reason =
    reject $ vcat [ text $ "(Nach Durchführung von: " ++ show (reverse done) ++ ")"
                  , text ""
                  , text $ "Nein. Operation '" ++ show op ++ "' ist nicht möglich. " ++ reason
                  ]

instance TreeOutputMonad (Marked Int) Verbose where
    treeOutput x = VerboseReporter $ lift $ output $ toPng x
instance TreeOutputMonad (Marked Int) OnFailure where
    treeOutput x = OnFailureReporter $ put x
instance OperationOutputMonad Verbose where
    operationOutput x = VerboseReporter $ modify (x:)
instance OperationOutputMonad OnFailure where
    operationOutput x = OnFailureReporter $ lift $ modify (x:)


instance Partial HeapSort Config Solution where
    report p (Config feedback numbers) = do
      inform $ vcat [ text "Führen Sie den Heapsort-Algorithmus auf folgendem Binärbaum durch:"
                    , text ""
                    ]
      output $ toPng $ T.fromList numbers
      inform $ vcat [ text ""
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
        inform TextConfig.noFeedbackDisclaimer

    initial p _ = Solution []

    total _ (Config None _) _ = do
        inform TextConfig.noFeedbackResult
    total p (Config feedback {- OnFailure or Verbose -} unsortedNumbers) (Solution operations) = do
        let t = decorate (T.fromList unsortedNumbers)
        let m = execute_ operations t
        t' <- case feedback of
                 OnFailure ->
                      flip evalStateT [] $ flip evalStateT t $ runOnFailure m
                 Verbose -> do
                      output $ toPng t
                      flip evalStateT [] $ runVerbose m
        unless (isSorted $ map value $ T.toList t') $ do
            when (feedback == OnFailure) $ do
               output $ toPng t'
            inform $ text $ "(Nach Durchführung von: " ++ show operations ++ ")"
            reject $ text "Nein. Baum entspricht nicht einer sortierten Liste."
        unless (all isMarked $ tail $ T.toList t') $ do
            when (feedback == OnFailure) $ do
               output $ toPng t'
            inform $ text $ "(Nach Durchführung von: " ++ show operations ++ ")"
            reject $ text "Nein. Es sind nicht alle Knoten markiert. Der Algorithmus würde hier noch nicht terminieren, obwohl die Elemente sortiert sind."
        inform TextConfig.ok



value (Marked x)   = x
value (Unmarked x) = x

isMarked (Marked _)   = True
isMarked (Unmarked _) = False

isSorted []         = True
isSorted [x]        = True
isSorted (x1:x2:xs) = x1 <= x2 && isSorted (x2:xs)

