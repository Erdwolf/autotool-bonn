{-# LANGUAGE DisambiguateRecordFields #-}

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

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad ( forM, foldM, guard )
import Data.Maybe ( maybeToList )
import Data.List ( minimum, partition )

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
        vs = concat $ replicate 2 $ S.toList s
    l <- with_vars vs $ term ( signature conf ) ( term_size conf )
    r <- with_vars vs $ term ( signature conf ) ( term_size conf ) 
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

with_vars vs a = a >>= insert_vars vs

insert_vars vs t = do
    foldM ( \ t v -> do
        p <- eins $ leaf_positions t
        return $ poke t p $ Variable v
      ) t vs

-- | introduce one wildcard
wild :: Identifier -> Term -> IO ( Term )
wild w t = do
    p <- eins $ drop 1 $ positions t
    return $ poke t p $ Apply w [] 

-- | a number of wildcards
many_wild :: Int -> Identifier 
          -> Term -> IO ( Term  )
many_wild n w t = foldM ( \ t _ -> wild w t ) t $ replicate n ()


term :: Signature -> Int -> IO ( Term  )
term sig s = 
    let ( sig0, sig1 ) = partition ( \ (f,a) -> a == 0 ) $ M.toList sig
    in  term' ( sig0, sig1 ) s

term' (sig0, sig1) s = do
   (f, a) <- eins $ if s <= 1 then sig0 else sig1
   if 0 == a 
      then return $ Apply f []
      else do
          ss <- distribute ( s - 1 ) a
          xs <- forM ss $ \ s -> term' (sig0, sig1) s
          return $ Apply f xs

-- | xs <- distribute t n  implies
--  sum xs == t  and  length xs == n  
-- watch it: runtime is Theta(t)
distribute :: Int -> Int -> IO [Int]
distribute t n = do
    ys <- forM [ 1 .. t ] $ \ i -> randomRIO (1,n)
    let fm = M.fromListWith (+) $ zip [1..n] ( repeat 0 ) ++ do
            y <- ys
            return ( y, 1 )
    return $ M.elems fm

