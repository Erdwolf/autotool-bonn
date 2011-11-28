{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections, FlexibleInstances,  NoMonomorphismRestriction #-}

module HeapSort.Central where

import HeapSort.Data
import HeapSort.Tree as T
import HeapSort.Semantics
import Tree.Class (ToTree(..))

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, (<>), hsep, toDoc, nest, ToDoc(..), docParen, fsep, (</>))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (Reporter, reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Inter.Types (OrderScore(..), ScoringOrder(Increasing), direct)

import Autolib.Dot.Dotty ( peng )

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

data Verbose   a = VerboseReporter { runVerbose :: Reporter a }
data OnFailure a = OnFailureReporter { runOnFailure :: StateT (Tree (Marked Int)) Reporter a }

instance Monad Verbose where
    return = VerboseReporter . return
    (VerboseReporter mx) >>= f = VerboseReporter $ mx >>= runVerbose . f
    fail x = VerboseReporter $ reject $ text x

instance Monad OnFailure where
    return = OnFailureReporter . return
    (OnFailureReporter mx) >>= f = OnFailureReporter $ mx >>= runOnFailure . f
    fail x = OnFailureReporter $ do
        t <- get
        lift $ peng $ toTree t
        lift $ reject $ text x

instance TreeOutputMonad (Marked Int) Verbose where
    treeOutput x = VerboseReporter $ peng $ toTree x
instance TreeOutputMonad (Marked Int) OnFailure where
    treeOutput x = OnFailureReporter $ put x


instance Partial HeapSort Config Solution where
    report p (Config feedback numbers) = do
      inform $ vcat [ text "Führen Sie den Heap-Sort-Algorithmus auf folgendem Binärbaum durch:"
                    ]

      peng (T.fromList numbers)

      when (feedback == None) $ do
        inform $ vcat [ text ""
                      , text "Hinweis: Bei dieser Aufgabe wird keine Rückmeldung über Korrektheit der Lösung gegeben."
                      , text "         Wenn eine Einsendung akzeptiert wird, heißt dies nicht, dass sie korrekt sein muss."
                      ]

    initial p _ = Solution []

    total p (Config feedback unsortedNumbers) (Solution operations) = do
       when (feedback /= None) $ do
           let t = T.fromList unsortedNumbers
           let m = do t' <- execute operations t
                      unless (isSorted $ T.toList t') $ do
                         fail "Baum entspricht nicht einer sortierten Liste."
           case feedback of
                  OnFailure ->
                       flip evalStateT (decorate t) $ runOnFailure m
                  Verbose ->
                       runVerbose m
       inform $ text "Ja."

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
