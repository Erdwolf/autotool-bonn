{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FlexibleInstances #-}
module HeapSort.Semantics where

import HeapSort.Operation
import HeapSort.Tree

import Data.Generics
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid
import Control.Applicative
import Data.List ((\\))


class Monad m => TreeOutputMonad a m where
    treeOutput :: Tree a -> m ()

instance Show a => TreeOutputMonad a IO where
    treeOutput = putStrLn . showTree

class Monad m => OperationOutputMonad a m where
    operationOutput :: Operation -> m ()

instance Show a => OperationOutputMonad a IO where
    operationOutput _ = return ()


execute path t = do
    undecorate `liftM` execute_ path (decorate t)

data Marked a = Marked a
              | Unmarked a
           deriving (Data, Typeable)

instance Show a => Show (Marked a) where
    show (Marked x)   = show x
    show (Unmarked x) = show x


decorate Empty = Empty
decorate (Branch x l r) =
    Branch (Unmarked x) (decorate l) (decorate r)

undecorate Empty = Empty
undecorate (Branch (Unmarked x) l r) =
    Branch x (undecorate l) (undecorate r)
undecorate (Branch (Marked x) l r) =
    Branch x (undecorate l) (undecorate r)

execute_ [] t = return t
execute_ (S k path:ops)  t = do
    operationOutput (S k path)
    (t',Any found) <- runWriterT $ sinken k path t
    when (not found) $ do
       fail $ "Knoten " ++ show k ++ " nicht gefunden."
    treeOutput t'
    execute_ ops t'
execute_ (T (k1,k2):ops) t = do
    operationOutput (T (k1,k2))
    t' <- tauschen k1 k2 t
    treeOutput t'
    execute_ ops t'


sinken k path t | hasId k t = do
    tell (Any True)
    swapAlong path t
sinken k path (Branch x l r) = do
    l' <- sinken k path l
    r' <- sinken k path r
    return (Branch x l' r')
sinken k path Empty = do
    return Empty

hasId k (Branch (Marked x) _ _)   = k == x
hasId k (Branch (Unmarked x) _ _) = k == x
hasId _ _                         = False


swapAlong [] t = return t

swapAlong (_:path) (Branch (Marked down) _ _) = do
    fail "Kann Knoten nicht nach links absenken. Der linke Kindsknoten ist bereits als abgespalten markiert."

swapAlong (L:path) (Branch (Unmarked down) l@(Branch (Marked up) ll lr) r) = do
    fail "Kann Knoten nicht nach links absenken. Der linke Kindsknoten ist bereits als abgespalten markiert."

swapAlong (L:path) (Branch (Unmarked down) l@(Branch (Unmarked up) ll lr) r) | not (isMaxHeap l) || not (isMaxHeap r) = do
    fail "Kann Knoten nicht absenken. Teilbaum erfüllt nicht die Heapeigenschaft."

swapAlong (L:path) (Branch (Unmarked down) l@(Branch (Unmarked up) ll lr) r) | down > up = do
    fail $ "Knoten " ++ show down ++ " kann nicht unter " ++ show up ++ " abgesenkt werden, da er größer ist."

swapAlong (L:path) (Branch (Unmarked down)   (Branch (Unmarked up) _ _) (Branch (Unmarked other) _ _)) | up < other = do
    fail $ "Knoten " ++ show down ++ " kann nicht unter " ++ show up ++ " abgesenkt werden, da " ++ show other ++ " größer ist."

swapAlong (L:path) (Branch (Unmarked down) l@(Branch (Unmarked up) ll lr) r) = do
    l' <- swapAlong path (Branch (Unmarked down) ll lr)
    return (Branch (Unmarked up) l' r)

swapAlong (R:path) (Branch (Unmarked down) l r@(Branch (Marked up) rl rr)) = do
    fail "Kann Knoten nicht nach rechts absenken. Der rechte Kindsknoten ist bereits als abgespalten markiert."

swapAlong (R:path) (Branch (Unmarked down) l r@(Branch (Unmarked up) rl rr)) | not (isMaxHeap l) || not (isMaxHeap r) = do
    fail "Kann Knoten nicht absenken. Teilbaum erfüllt nicht die Heapeigenschaft."

swapAlong (R:path) (Branch (Unmarked down) l r@(Branch (Unmarked up) rl rr)) | down > up = do
    fail $ "Knoten " ++ show down ++ " kann nicht unter " ++ show up ++ " abgesenkt werden, da er größer ist."

swapAlong (R:path) (Branch (Unmarked down) (Branch (Unmarked other) _ _) (Branch (Unmarked up) _ _)) | up < other = do
    fail $ "Knoten " ++ show down ++ " kann nicht unter " ++ show up ++ " abgesenkt werden, da " ++ show other ++ " größer ist."

swapAlong (R:path) (Branch (Unmarked down) l r@(Branch (Unmarked up) rl rr)) = do
    r' <- swapAlong path (Branch (Unmarked down) rl rr)
    return (Branch (Unmarked up) l r')

swapAlong (L:_) (Branch _ Empty _)  = do
    fail "Ein Knoten kann nur nach links absinken, wenn er einen linken Unterbaum hat."

swapAlong (R:_) (Branch _ _ Empty)  = do
    fail "Ein Knoten kann nur nach rechts absinken, wenn er einen rechten Unterbaum hat."

swapAlong (_:_) Empty = do
    fail "Ein Knoten kann nur absinken, wenn er Kindsknoten hat."



tauschen k1 k2 t@(Branch (Marked top) l r) = do
    fail "Es kann nicht mehr getauscht werden, da bereits alle Knoten als abgespalten markiert sind."

tauschen k1 k2 t@(Branch (Unmarked top) l r) = do
    when (not (isMaxHeap t)) $ do
        fail "Es kann erst getauscht werden, wenn der gesamte Baum die Heapeigenschaft erfüllt."
    when (not (k1 == top || k2 == top)) $ do
        fail "Einer der beiden zu tauschenden Knoten muss die Wurzel sein."

    let lastUnmarked = last [ u | Unmarked u <- toList t ]

    when (not (k1 == lastUnmarked || k2 == lastUnmarked)) $ do
        fail "Es kann nur mit dem letzten noch nicht als abgespalten markierten Knoten getauscht werden."

    let replaceNode (Branch (Unmarked x) l r) | x == lastUnmarked = Branch (Marked top) l r
        replaceNode t                                             = t

    return $ uncurry (Branch (Unmarked lastUnmarked))
           $ everywhere (mkT replaceNode) (l,r)



isMaxHeap :: Ord a => Tree (Marked a) -> Bool
isMaxHeap t | emptyOrMarked t = True
isMaxHeap (Branch (Unmarked x) l r) | emptyOrMarked l && emptyOrMarked r = True
isMaxHeap (Branch (Unmarked x) l@(Branch (Unmarked x1) _ _) r) | emptyOrMarked r  =
    x1 <= x     &&
    isMaxHeap l
isMaxHeap (Branch (Unmarked x) l r@(Branch (Unmarked x2) _ _)) | emptyOrMarked l =
    x2 <= x     &&
    isMaxHeap r
isMaxHeap (Branch (Unmarked x) l@(Branch (Unmarked x1) _ _) r@(Branch (Unmarked x2) _ _)) =
    x1 <= x     &&
    x2 <= x     &&
    isMaxHeap l &&
    isMaxHeap r

emptyOrMarked Empty = True
emptyOrMarked (Branch (Marked _) _ _) = True
emptyOrMarked _ = False
