{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections #-}

module HeapSort.Central where

import HeapSort.Data
import HeapSort.Tree as T
import HeapSort.Semantics
import Tree.Class (ToTree(..))

import Debug ( debug )

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc (derives, makeToDoc, text, vcat, (<>), hsep, toDoc, nest, ToDoc(..))
import Autolib.Reader (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (Reporter, reject, inform)
import qualified Autolib.Reporter.IO.Type (reject, inform)
import Inter.Types (OrderScore(..), ScoringOrder(..), direct)

import Autolib.Dot.Dotty ( peng )

import Data.Typeable (Typeable)
import Control.Monad (when,unless)
import qualified Data.Tree

rejectIO = Autolib.Reporter.IO.Type.reject
informIO = Autolib.Reporter.IO.Type.inform

data HeapSort = HeapSort deriving Typeable

$(derives [makeReader, makeToDoc] [''HeapSort])

make_fixed = direct HeapSort $ Config True [5,4,2,1,3,6]

instance OrderScore HeapSort where
    scoringOrder h = Increasing

instance Verify HeapSort Config where
    verify _ cfg = do
        return ()

newtype Wrapper a = Wrapper { runWrapper :: Reporter a }
instance  Monad Wrapper where
    return = Wrapper . return
    (Wrapper mx) >>= f = Wrapper $ mx >>= runWrapper . f
    fail x = Wrapper (reject $ text x)

instance Partial HeapSort Config Solution where
    report p (Config giveFeedback numbers) = do
      inform $ vcat [ text "Führen Sie den Heap-Sort-Algorithmus auf folgendem Binärbaum durch:"
                    ]

      peng (toDataTree (T.fromList numbers))

      unless giveFeedback $ do
        inform $ vcat [ text ""
                      , text "Hinweis: Bei dieser Aufgabe wird keine Rückmeldung über Korrektheit der Lösung gegeben."
                      , text "         Wenn eine Einsendung akzeptiert wird, heißt dies nicht, dass sie korrekt sein muss."
                      ]

    initial p _ = Solution []

    total p (Config giveFeedback unsortedNumbers) (Solution operations) = do
       t <- runWrapper $ execute operations (T.fromList unsortedNumbers)
       inform $ text "Ja."

instance ToTree (T.Tree Int) where
  toTree = Data.Tree.unfoldTree uf
    where
      uf (T.Branch x Empty Empty) = (show x,[])
      uf (T.Branch x l     Empty) = (show x,[l])
      uf (T.Branch x Empty r    ) = (show x,[r])
      uf (T.Branch x l     r    ) = (show x,[l,r])
