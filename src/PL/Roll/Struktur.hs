module PL.Roll.Struktur where

import PL.Signatur
import PL.Struktur

import Autolib.Util.Zufall
import Autolib.Util.Wort

import Autolib.FiniteMap
import qualified Data.Map as M
import Autolib.Set
import qualified Data.Set as S

import Control.Monad ( forM )

mutate :: Ord u => Struktur u -> IO ( Struktur u )
mutate s = do
    action <- eins [ mutate_function , mutate_predicate ]
    action s

mutate_function s = 
    if M.null ( functions s ) then return s else do
        ( n, Function f ) <- eins $ M.toList $ functions s
        ( arg, _ ) <- eins $ M.toList f
        res <- eins $ S.toList $ universum s
        let f' = M.insert arg res f
        return $ s { functions = M.insert n ( Function f' )
                               $ functions s }
          
mutate_predicate s = 
    if M.null ( predicates s ) then return s else do
        ( n, Predicate p ) <- eins $ M.toList $ predicates s
        if S.null p then return s -- because we don't know
                                  -- the arity here
            else do             
                let a = length $ head $ S.toList p
                arg <- Autolib.Util.Zufall.someIO 
                       ( S.toList $ universum s ) a
                let p' = if S.member arg p
                         then S.delete arg p   
                         else S.insert arg p     
                return $ s { predicates = M.insert n 
                                          ( Predicate p' )
                               $ predicates s }

permute :: Ord u => Struktur u -> IO ( Struktur u )
permute s = do
    let u = S.toList $ universum s
    v <- permutation u
    let m = M.fromList $ zip u v
    return $ apply ( m M.! ) s

apply :: Ord u 
        => ( u -> u )
        -> Struktur u 
        -> Struktur u
apply f s = Struktur 
    { universum = S.map f $ universum s
    , predicates = M.map ( \ ( Predicate args ) -> 
        Predicate $ S.map ( map f ) args ) $ predicates s
    , functions = M.map ( \ ( Function fun ) -> 
        Function $ M.fromList $ map ( \ (arg, res) -> 
           ( map f arg, f res ) ) $ M.toList fun) 
        $ functions s
    }               

-- | für gegebene Signatur,
-- erzeuge Struktur mit gegebenem Universum
roll :: Ord u 
     =>   Signatur -> Set u -> IO ( Struktur u )
roll sig u = do
    fs <- forM ( M.toList $ funktionen sig ) $ \ (n,a) -> do
        f <- function u a
        return ( n, f )
    ps <- forM ( M.toList $ relationen sig ) $ \ (n,a) -> do
        p <- predicate u a
        return ( n, p )
    return $ Struktur
        { universum = u
        , predicates = M.fromList ps              
        , functions = M.fromList fs               
        }              
  
predicate :: Ord u 
         => Set u -- ^  Universum
         -> Int -- ^ Stelligkeit
         -> IO ( Predicate u )
predicate u a = do
    items <- forM ( alle ( S.toList u ) a ) $ \ arg -> do
        res <- eins [ False, True ]
        return ( arg, res )
    return $ Predicate $ S.fromList 
           $ map fst $ filter snd items
  
function :: Ord u 
         => Set u -- ^  Universum
         -> Int -- ^ Stelligkeit
         -> IO ( Function u )
function u a = do
    pairs <- forM ( alle ( S.toList u ) a ) $ \ arg -> do
        res <- eins ( S.toList u ) 
        return ( arg, res )
    return $ Function $ M.fromList pairs     
    
    
