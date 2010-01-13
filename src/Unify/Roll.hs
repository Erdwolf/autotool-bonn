{-# language DisambiguateRecordFields #-}

module Unify.Roll where

import Prolog.Data
import Prolog.Unify

import Unify.Instance
import Unify.Config

import Autolib.Util.Zufall
import Autolib.Util.Sort
import Autolib.Size
import Autolib.FiniteMap
import Autolib.Set

import Control.Monad ( foldM, guard )
import Data.Maybe ( maybeToList )
import Data.List ( minimum )

roll :: Config 
     -> IO ( Instance  )
roll conf = do
    insts <- mapM inst $ replicate ( num_candidates conf ) conf
    let value i = ( sum $ do ( v, t ) <- fmToList $ unifier i ; return $ size t )
                * minimum [ size $ left i , size $ right i ]
    case sortBy ( negate . value ) $ concat $ map  maybeToList insts of
        [] -> error "konnte keine Aufgabeninstanz generieren"
        i : _ -> return i

inst :: Config -> IO ( Maybe ( Instance ))
inst conf = do
    let s = Unify.Config.variables conf
    l <- term s ( signature conf ) ( term_size conf )
    r <- term s ( signature conf ) ( term_size conf ) 
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
            return $ cardinality ( Prolog.Data.variables t ) > 0
        return $ Instance { wildcard = w , left = wl, right = wr, unifier = u }

-- | introduce one wildcard
wild :: c -> Term -> IO ( Term )
wild w t = do
    ( p, s ) <- eins $ drop 1 $ positions t
    return $ poke t ( p, Apply w [] )

-- | a number of wildcards
many_wild :: Int -> Identifier 
          -> Term -> IO ( Term  )
many_wild n w t = foldM ( \ t _ -> wild w t ) t $ replicate n ()

term :: [Identifier] 
     -> Signature -> Int -> IO ( Term  )
term vars sig s = 
   if s <= 1 then do
       eins $ map Variable vars ++ map ( \ f -> Apply f [] ) ( nullary sig )
   else do
       sl <- randomRIO ( 1, s - 2 )
       l <- term vars sig $ sl
       r <- term vars sig $ s - 1 - sl
       f <- eins $ binary sig
       return $ Apply f [ l, r ]

