module Program.List.Store 

( Key -- abstract
, Contents (..)
, StoreT, ReporterST, empty
, row, scalar, void
, build, update, access
, make, unmake
)

where

import qualified Program.List.Value as V

import Autolib.FiniteMap
import Autolib.Reporter
import Control.Monad.State

data Key = Key Int deriving ( Eq, Ord )
data Contents = Scalar Integer | Void
              | Row { typeof :: V.Type, contents :: [ Key ] }
type Store = FiniteMap Key Contents 
type StoreT = StateT Store 
type ReporterST = StoreT Reporter

empty :: Store 
empty = emptyFM

scalar :: Monad m => Integer -> StoreT m Key
scalar i = build $ Scalar i

row :: Monad m => V.Type -> [ Key ] -> StoreT m Key
row ty ks = build $ Row ty ks

void :: Monad m => StoreT m Key
void = build $ Void

build :: Monad m => Contents -> StoreT m Key
build c = do
    st <- get
    let top = Key $ 1 + sizeFM st
    put $ addToFM st top c
    return top

update :: Monad m => Key -> Contents -> StoreT m ()
update k c = do
    st <- get
    put $ addToFM st k c
    return ()

access :: Monad m => Key -> StoreT m Contents
access k = do
    st <- get
    let Just c = lookupFM st k
    return c

make :: Monad m => V.Value -> StoreT m Key
make v = case v of
    V.Scalar s -> scalar s
    V.Collect {} -> do
        ks <- mapM make $ V.contents v
        row ( V.typeof v ) ks

unmake :: Monad m => Key -> StoreT m V.Value
unmake k = do
    c <- access k
    case c of
        Scalar s -> return $ V.Scalar s
        Row {}   -> do
             xs <- mapM unmake $ contents c
             return $ V.Collect ( typeof c ) xs
