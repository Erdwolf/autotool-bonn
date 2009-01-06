{-# language DisambiguateRecordFields #-}

module Unify.Roll where

import Unify.Instance
import Unify.Config

import Autolib.TES.Term
import Autolib.TES.Position
import Autolib.TES.Binu
import Autolib.TES.Unify
import Autolib.TES.Identifier

import Autolib.Util.Zufall
import Autolib.Util.Sort
import Autolib.Size
import Autolib.FiniteMap
import Autolib.Set

import Control.Monad ( foldM, guard )
import Data.Maybe ( maybeToList )
import Data.List ( minimum )

roll :: InstanceC v c 
     => Config v c
     -> IO ( Instance v c )
roll conf = do
    insts <- mapM inst $ replicate ( num_candidates conf ) conf
    let value i = ( sum $ do ( v, t ) <- fmToList $ unifier i ; return $ size t )
                * minimum [ size $ left i , size $ right i ]
    case sortBy ( negate . value ) $ concat $ map  maybeToList insts of
        [] -> error "konnte keine Aufgabeninstanz generieren"
        i : _ -> return i

inst :: InstanceC v c 
     => Config v c -> IO ( Maybe ( Instance v c ))
inst conf = do
    let s = mkSet $ variables conf
    l <- term ( variables conf ) ( signature conf ) ( term_size conf )
    r <- term ( variables conf ) ( signature conf ) ( term_size conf ) 
    let w = Unify.Config.wildcard conf
    wl <- many_wild ( num_wildcards conf ) w l
    wr <- many_wild ( num_wildcards conf ) w r
    return $ do
        -- guard $ s == union ( vars wl ) ( vars wr ) 
        u <- mgu l r
        guard $ or $ do
            ( v, t ) <- fmToList u
            return $ size t > 1 
        guard $ or $ do
            ( v, t ) <- fmToList u
            return $ cardinality ( vars t ) > 0
        return $ Instance { wildcard = w , left = wl, right = wr, unifier = u }

-- | introduce one wildcard
wild :: c -> Term v c -> IO ( Term v c )
wild w t = do
    ( p, s ) <- eins $ drop 1 $ positions t
    return $ poke t ( p, Node w [] )

-- | a number of wildcards
many_wild :: Int -> c -> Term v c -> IO ( Term v c )
many_wild n w t = foldM ( \ t _ -> wild w t ) t $ replicate n ()

term :: InstanceC v c 
          => [v] -> Binu c -> Int -> IO ( Term v c )
term vars sig s = 
   if s <= 1 then do
       eins $ map Var vars ++ map ( \ f -> Node f [] ) ( nullary sig )
   else do
       sl <- randomRIO ( 1, s - 2 )
       l <- term vars sig $ sl
       r <- term vars sig $ s - 1 - sl
       f <- eins $ binary sig
       return $ Node f [ l, r ]

