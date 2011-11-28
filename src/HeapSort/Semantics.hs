{-# LANGUAGE DeriveDataTypeable #-}
module HeapSort.Semantics where

import HeapSort.Operation
import HeapSort.Tree

import Data.Generics
import Control.Monad.State
import Control.Monad.Writer
import Data.Monoid
import Control.Applicative
import Data.List ((\\))

{-
execute path t = do
    (t',_) <- runStateT (undecorate =<< execute_ path =<< decorate t) [{- Markierte Knoten -}]
    return t'
-}

execute path t = undecorate =<< execute_ path =<< decorate t

data Marked a = Marked a
              | Unmarked a
           deriving (Data, Typeable)

{-
instance Eq a => Eq (Marked a) where
instance Ord a => Ord (Marked a) where
    Marked _   <= _          = True
    _          <= Marked a   = False
    Unmarked a <= Unmarked b = a <= b
-}

instance Show a => Show (Marked a) where
    show (Marked x)   = show x
    show (Unmarked x) = show x

{-
isMarked (Marked _) = True
isMarked _          = False
-}

decorate Empty = return Empty
decorate (Branch x l r) = do
    l' <- decorate l
    r' <- decorate r
    return (Branch (Unmarked x) l' r')

undecorate Empty = return Empty
undecorate (Branch (Unmarked x) l r) = do
    l' <- undecorate l
    r' <- undecorate r
    return (Branch x l' r')
undecorate (Branch (Marked x) l r) = do
    l' <- undecorate l
    r' <- undecorate r
    return (Branch x l' r')

execute_ [] t = return t
execute_ (Sinken k path:ops)  t = do
    (t',Any found) <- runWriterT $ sinken k path t
    when (not found) $ do
       fail $ "Knoten " ++ show k ++ " nicht gefunden."
    execute_ ops t'
execute_ (Tauschen k1 k2:ops) t = do
    t' <- tauschen k1 k2 t
    {-
    ((t',Any found1),Any found2) <- runWriterT $ runWriterT $ tauschen k1 k2 t
    when (not found1) $ do
       fail $ "Knoten " ++ show k1 ++ " nicht gefunden."
    when (not found2) $ do
       fail $ "Knoten " ++ show k2 ++ " nicht gefunden."
    -}
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
    fail "Kann Knoten nicht nach links absenken. Der linke Kindsknoten ist bereits markiert."

swapAlong (L:path) (Branch (Unmarked down) l@(Branch (Marked up) ll lr) r) = do
    fail "Kann Knoten nicht nach links absenken. Der linke Kindsknoten ist bereits markiert."

swapAlong (L:path) (Branch (Unmarked down) l@(Branch (Unmarked up) ll lr) r) = do
    when (not (isMaxHeap l) || not (isMaxHeap r)) $
        fail "Kann Knoten nicht absenken. Teilbaum erfüllt nicht die Heapeigenschaft."
    when (down > up) $ do
        fail $ "Knoten " ++ show down ++ " kann nicht unter " ++ show up ++ " abgesenkt werden, da er größer ist."
    l' <- swapAlong path (Branch (Unmarked down) ll lr)
    return (Branch (Unmarked up) l' r)

swapAlong (R:path) (Branch (Unmarked down) l r@(Branch (Marked up) rl rr)) = do
    fail "Kann Knoten nicht nach rechts absenken. Der rechte Kindsknoten ist bereits markiert."

swapAlong (R:path) (Branch (Unmarked down) l r@(Branch (Unmarked up) rl rr)) = do
    when (not (isMaxHeap l) || not (isMaxHeap r)) $
        fail "Kann Knoten nicht absenken. Teilbaum erfüllt nicht die Heapeigenschaft."
    when (down > up) $ do
        fail $ "Knoten " ++ show down ++ " kann nicht unter " ++ show up ++ " abgesenkt werden, da er größer ist."
    r' <- swapAlong path (Branch (Unmarked down) rl rr)
    return (Branch (Unmarked up) l r')

swapAlong (L:_) (Branch _ Empty _)  = do
    fail "Ein Knoten kann nur nach links absinken, wenn er einen linken Unterbaum hat."

swapAlong (R:_) (Branch _ _ Empty)  = do
    fail "Ein Knoten kann nur nach rechts absinken, wenn er einen rechten Unterbaum hat."

swapAlong (_:_) Empty = do
    fail "Ein Knoten kann nur absinken, wenn er Kindsknoten hat."



tauschen k1 k2 t@(Branch (Marked top) l r) = do
    fail "Es kann nicht mehr getauscht werden, da bereits alle Knoten markiert sind."

tauschen k1 k2 t@(Branch (Unmarked top) l r) = do
    when (not (isMaxHeap t)) $ do
        fail "Es kann erst getauscht werden, wenn der gesamte Baum die Heapeigenschaft erfüllt."
    when (not (k1 == top || k2 == top)) $ do
        fail "Einer der beiden zu tauschenden Knoten muss die Wurzel sein."

    let lastUnmarked = last [ u | Unmarked u <- toList t ]

    when (not (k1 == lastUnmarked || k2 == lastUnmarked)) $ do
        fail "Es kann nur mit dem letzten unmarkierten Knoten getauscht werden."

    let replaceNode (Branch (Unmarked x) l r) | x == lastUnmarked = Branch (Marked top) l r
        replaceNode t                                             = t

    return $ uncurry (Branch (Unmarked lastUnmarked))
           $ everywhere (mkT replaceNode) (l,r)



isMaxHeap :: Ord a => Tree (Marked a) -> Bool
isMaxHeap Empty = True
isMaxHeap (Branch (Unmarked x) Empty Empty) = True
isMaxHeap (Branch (Unmarked x) l@(Branch (Unmarked x1) _ _) Empty) =
    x1 <= x     &&
    isMaxHeap l
isMaxHeap (Branch (Unmarked x) Empty r@(Branch (Unmarked x2) _ _)) =
    x2 <= x     &&
    isMaxHeap r
isMaxHeap (Branch (Unmarked x) l@(Branch (Unmarked x1) _ _) r@(Branch (Unmarked x2) _ _)) =
    x1 <= x     &&
    x2 <= x     &&
    isMaxHeap l &&
    isMaxHeap r
isMaxHeap (Branch _ _ _) = True

