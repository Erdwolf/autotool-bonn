{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, OverlappingInstances, DeriveDataTypeable, StandaloneDeriving, TypeSynonymInstances, TupleSections, FlexibleInstances,  NoMonomorphismRestriction #-}
module AVLBaumBonn.Central where

import Challenger.Partial (Verify(..), Partial(..))
import Autolib.ToDoc -- (derives, makeToDoc, Doc(..), text, vcat, hcat, ($$),  (<>), (<+>), hsep, toDoc, nest, ToDoc(..), docParen, fsep, (</>), empty)
import Autolib.Reader -- (makeReader, Reader(..), {- only needed inside derived code: -} readerParenPrec, my_reserved, pzero, (<|>))
import Autolib.Reporter (Reporter, reject, inform)

import Autolib.Dot.Dot
import Autolib.Hash
import qualified Autolib.Dot.Graph
import qualified Autolib.Dot.Node
import qualified Autolib.Dot.Edge
import qualified Autolib.Dot (peng)

import Autolib.Size (Size(size))
import Inter.Types (OrderScore(..), ScoringOrder(Increasing), direct)

import Data.Typeable (Typeable)
import Control.Monad (when,unless,guard)
import Data.List (zip5, transpose, intersperse)
import Data.Traversable (traverse)
import Data.Tree
import Control.Monad.State (evalState, get, put)

import qualified Baum.Such.Generate
import qualified Baum.Such.Op
import qualified Baum.Such.Class
import qualified Baum.AVL.Type
import qualified Baum.AVL.Ops

import Baum.AVL.Type (isLeaf, left, right, key)


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
--
instance ToDot (Baum.AVL.Type.AVLTree Int) where
    toDotProgram _ = Dot
    toDotOptions _ = unwords [ "-Gordering=out", "-Gnodesep=0" ]
    toDot avltree =
        Autolib.Dot.Graph.Type
              { Autolib.Dot.Graph.directed = True
              , Autolib.Dot.Graph.name = "foo"
              , Autolib.Dot.Graph.nodes = nodes
              , Autolib.Dot.Graph.edges = edges
              , Autolib.Dot.Graph.attributes = []
              }
      where
        numbered =
          flip evalState 0 $ flip traverse (toTree avltree) $ \x -> do
            i <- get; put (succ i)
            return (i, x)

        nodes =
          flip map (flatten numbered) $ \(i, x) ->
            Autolib.Dot.Node.blank
              { Autolib.Dot.Node.label = Just (maybe "" show x)
              , Autolib.Dot.Node.shape = Just "plaintext"
              , Autolib.Dot.Node.ident = show i
              }

        edges = do
            src@(Node (i, _) _) <- subtrees numbered
            dst@(Node (j, x) _) <- subForest src
            return $ Autolib.Dot.Edge.blank
                       { Autolib.Dot.Edge.from     = show i
                       , Autolib.Dot.Edge.to       = show j
                       , Autolib.Dot.Edge.directed = True
                       , Autolib.Dot.Edge.color    = maybe (Just "white") (\_->Nothing) x
                       }

        subtrees t = t : concatMap subtrees (subForest t)

toTree t = Data.Tree.unfoldTree uf t
   where
       uf t |       isLeaf t       = (Nothing,[])
            | isLeaf l && isLeaf r = (Just k,[])
            |             isLeaf r = (Just k,[l,r])
            | isLeaf l             = (Just k,[l,r])
            |       otherwise      = (Just k,[l,r])
        where k = key t
              l = left t
              r = right t

instance Hash (Baum.AVL.Type.AVLTree Int) where
    hash = hash . toTree


peng :: Baum.AVL.Type.AVLTree Int -> Reporter ()
peng = Autolib.Dot.peng


-------------------------

data Feedback = Always
              | OnlyOnCompletion
           deriving (Typeable)

$(derives [makeReader, makeToDoc] [''Feedback])

data AVLTreeBonn = Node Int AVLTreeBonn AVLTreeBonn
                 | Empty
  deriving (Typeable)

$(derives [makeReader, makeToDoc] [''AVLTree])

bonnifyTree :: Baum.AVL.Type.AVLTree Int -> AVLTreeBonn
bonnifyTree t | Baum.AVL.Type.isLeaf t = Empty
bonnifyTree t                          = Node (Baum.AVL.Type.key t)
                                              (Baum.AVL.Type.left t)
                                              (Baum.AVL.Type.right t)

debonnifyTree :: AVLTreeBonn -> AVLTree Baum.AVL.Type.AVLTree Int
debonnifyTree (Node x l r) = Baum.AVL.Type.branch l x r
debonnifyTree Empty        = Baum.AVL.Type.leaf



data Config = Config
     { feedback :: Feedback
     , startTree :: Baum.AVL.Type.AVLTree Int
     , operations :: [Op]
     , finalTree :: Baum.AVL.Type.AVLTree Int
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
    , startTree = nil
    , operations = [Baum.Such.Op.Any]
    , finalTree = Baum.AVL.Type.branch nil 42 nil
    }
    where nil = Baum.AVL.Type.leaf

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
    report _ (Config _fb start plan end) = do
       if isLeaf start
          then do
            inform $ vcat [ text "Auf einen leeren Baum sollen diese Operationen angewendet werden"
                          , text "(wobei Sie jedes Any durch ein MyInsert mit dem einzufügenden Element als Argument ersetzen sollen):"
                          ]
          else do
            inform $ text "Auf den Baum:"
            peng start
            inform $ vcat [ text "sollen diese Operationen angewendet werden"
                          , text "(wobei Sie jedes Any durch ein MyInsert mit dem einzufügenden Element als Argument ersetzen sollen):"
                          ]
       inform $ vcat [ nest 4 $ niceOps (map convertOp plan)
                     , text ""
                     , text "so dass dieser Baum entsteht:"
                     ]
       peng end
       inform $ text "<span style='color:red'>Hinweis: Die zum Rebalancieren des Baumes nötigen <em>Rotationen</em> werden beim Einfügen automatisch durchgeführt. Sie müssen diese <em>nicht</em> mit angeben.</span>"

    initial _ (Config _ _ plan _) =
        OpList (map convertOp plan)

    total _ (Config fb start plan end) (OpList ops) = do
        c <- steps start (map convertOp plan) ops []
        if c == end
           then inform $ text "Ja."
           else rejectTreeAlways c ops $ text "Resultat stimmt nicht mit Aufgabenstellung überein."

      where
        rejectTree :: Baum.AVL.Type.AVLTree Int -> [AVLOp] -> Doc -> Reporter a
        rejectTree b ops reason = do
            case fb of
                OnlyOnCompletion -> do
                    reject $ text "Nein." <+> reason
                Always -> do
                    rejectTreeAlways b ops reason

        rejectTreeAlways b ops reason = do
            inform $ text "Nein." <+> reason
            inform $ text $ "<b>Tatsächlicher Baum*  <->  Ziel-Baum</b>"
            inform $ text "*Ausgeführte Operationen:" <+> toDoc ops
            peng b   -- Tatsächlicher Baum
            peng end -- Erwarteter Baum
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

