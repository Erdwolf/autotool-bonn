{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections, FlexibleInstances,  NoMonomorphismRestriction #-}
module AVLBaumBonn.Central where

import qualified TextConfig

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc -- (derives, makeToDoc, Doc(..), text, vcat, hcat, ($$),  (<>), (<+>), hsep, toDoc, nest, ToDoc(..), docParen, fsep, (</>), empty)
import Autolib.Reader -- (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (Reporter, reject, inform)

import Autolib.Dot.Dot
import Autolib.Hash
import qualified Autolib.Dot.Graph
import qualified Autolib.Dot.Node
import qualified Autolib.Dot.Edge

import Autolib.Size (Size(size))
import Inter.Types (OrderScore(..), ScoringOrder(Increasing), direct)

import Data.Typeable (Typeable)
import Control.Monad (when,unless,guard)
import Data.List (zip5, transpose, intersperse)

import qualified Baum.Such.Generate
import qualified Baum.Such.Op
import qualified Baum.Such.Class
import qualified Baum.AVL.Type
import qualified Baum.AVL.Ops

import Data.Traversable (traverse)
import Control.Monad.State (evalState, get, put)
import Baum.AVL.Type (isLeaf, left, right, key)

import AVLBaumBonn.GraphViz
import AVLBaumBonn.Conversion

instance Baum.Such.Class.Such Baum.AVL.Type.AVLTree where
    empty = Baum.AVL.Type.leaf
    isEmpty = Baum.AVL.Type.isLeaf

    contains = Baum.AVL.Ops.contains
    insert = Baum.AVL.Ops.insert
    delete = error "Delete is für AVL-Bäume nicht implementiert"

    equal = (==)
    contents =  Baum.AVL.Type.inorder

data AVLBaum = AVLBaum deriving Typeable

$(derives [makeReader, makeToDoc] [''AVLBaum])

-------------------------

data Feedback = Always
              | OnlyOnCompletion
              | None
           deriving (Typeable, Eq)

$(derives [makeReader, makeToDoc] [''Feedback])


data Config = Config
     { feedback :: Feedback
     , startTree :: AVLTreeBonn
     , operations :: [Baum.Such.Op.Op Int]
     , finalTree :: AVLTreeBonn
     }
  deriving (Typeable)


$(derives [makeReader, makeToDoc] [''Config])


instance OrderScore AVLBaum where
    scoringOrder h = Increasing

instance Verify AVLBaum Config where
    verify _ cfg = do
        return ()

make_fixed = direct AVLBaum $ Config
    { feedback = Always
    , startTree = Empty
    , operations = [Baum.Such.Op.Any]
    , finalTree = Node 42 Empty Empty
    }

data AVLOp = Insert Int
           | MyInsert Int
           | Any

convertOp :: Baum.Such.Op.Op Int -> AVLOp
convertOp (Baum.Such.Op.Insert x) = Insert x
convertOp Baum.Such.Op.Any        = Any
convertOp _                       = error "Operation not valid on AVL Tree"

newtype OpList = OpList [AVLOp] deriving (Typeable)

$(derives [makeReader] [''AVLOp])

instance ToDoc AVLOp where
    toDoc (Insert a)   = text "Insert(" <> toDoc a <> text ")"
    toDoc (MyInsert a) = text "MyInsert(" <> toDoc a <> text ")"
    toDoc Any          = text "Any"

instance Reader OpList where
    reader = do
        ops <- reader
        return (OpList ops)

instance ToDoc OpList where
    toDoc (OpList ops) = text (show ops)

instance Size OpList where
    size (OpList ops) = length ops

instance Partial AVLBaum Config OpList where
    report _ (Config fb startB plan endB) = do
       let start = debonnifyTree startB
           end   = debonnifyTree endB
       if isLeaf start
          then do
            inform $ vcat [ text "Auf einen leeren Baum sollen diese Operationen angewendet werden"
                          , text "(wobei Sie jedes Any durch ein MyInsert mit dem einzufügenden Element als Argument ersetzen sollen):"
                          ]
          else do
            inform $ vcat [ text "Auf den Baum:"
                          , text ""
                          , text (toPng start)
                          , text ""
                          , text "sollen diese Operationen angewendet werden"
                          , text "(wobei Sie jedes Any durch ein MyInsert mit dem einzufügenden Element als Argument ersetzen sollen):"
                          ]
       inform $ vcat [ nest 4 $ niceOps (map convertOp plan)
                     , text ""
                     , text "so dass dieser Baum entsteht:"
                     , text ""
                     , text (toPng end)
                     ]
       when (fb == None) $ do
          inform TextConfig.noFeedbackDisclaimer
       inform $ text "<span style='color:red'>Hinweis: Die zum Rebalancieren des Baumes nötigen <em>Rotationen</em> werden beim Einfügen automatisch durchgeführt. Sie müssen diese <em>nicht</em> mit angeben.</span>"

    initial _ (Config _ _ plan _) =
        OpList (map convertOp plan)

    total _ (Config None _ _ _) _ = do
        inform TextConfig.noFeedbackResult

    total _ (Config fb startB plan endB) (OpList ops) = do
        c <- steps start (map convertOp plan) ops []
        if c == end
           then inform TextConfig.ok
           else rejectTreeAlways c ops $ text "Resultat stimmt nicht mit Aufgabenstellung überein."

      where
        start = debonnifyTree startB
        end   = debonnifyTree endB

        rejectTree :: Baum.AVL.Type.AVLTree Int -> [AVLOp] -> Doc -> Reporter a
        rejectTree b ops reason = do
            case fb of
                OnlyOnCompletion -> do
                    reject $ text "Nein." <+> reason
                Always -> do
                    rejectTreeAlways b ops reason

        rejectTreeAlways b ops reason = do
            inform $ vcat [ text "Nein." <+> reason
                          , text ""
                          , text $ "<b>Tatsächlicher Baum*  <->  Ziel-Baum</b>"
                          , text "*Ausgeführte Operationen:" <+> toDoc ops
                          , hcat [ text $ toPng b   -- Tatsächlicher Baum
                                 , text $ toPng end -- Erwarteter Baum
                                 ]
                          ]
            reject $ text ""

        step b op = do
            c <- case op of
             MyInsert a -> insert a
             Insert a   -> insert a
             _          -> reject $ text "Operation ist unbekannt"
            return c
          where
            insert a = do
              -- Falls der einzufügende Knoten schon im Baum ist,
              -- lässt diese Operation den Baum unverändert.
              if b `Baum.AVL.Ops.contains` a
                 then return b
                 else return $ Baum.AVL.Ops.insert b a

        steps b [] [] _ = return b
        steps b [] send done = rejectTree b (reverse done) $ vcat
                                  [ text "Sie wollen noch diese Operationen ausführen:"
                                  , nest 4 $ niceOps send
                                  , text "es sind aber keine mehr zugelassen."
                                  ]
        steps b plan [] done = rejectTree b (reverse done) $ vcat
                                  [ text "Es müssen noch diese Operationen ausgeführt werden:"
                                  , nest 4 $ niceOps plan
                                  ]
        steps b (p : plan) (s : send) done = do
            conforms p s
            c <- step b s
            steps c plan send (s : done)
          where
            conforms _ Any = do
                rejectTree b (reverse done) $ text "Sie sollen Any durch eine Operation ersetzen."
            conforms Any _ = return ()
            conforms (Insert x) (Insert y)   | x == y = return ()
            conforms (Insert x) (MyInsert y) | x == y = return ()
            conforms op@(Insert _) _ = do
                rejectTree b (reverse done) $ text "Die Operation" <+> toDoc op <+> text "soll nicht geändert werden." 

niceOps [] = text "[]"
niceOps (x:xs) = vcat [ text "[" <+> toDoc x
                      , vcat [ text "," <+> toDoc x' | x' <- xs ]
                      , text "]"
                      ]

