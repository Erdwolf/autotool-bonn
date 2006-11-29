{-# OPTIONS -fglasgow-exts #-}

module Lambda.Quiz (generator) where

import Lambda.Type
import Lambda.Step
import qualified Lambda.Roll
import qualified Lambda.Derive.Instance as I
import qualified Lambda.Derive.Config as C


import Autolib.TES.Identifier
import Autolib.Set
import Autolib.Size
import Autolib.Schichten
import Autolib.Reporter
import Autolib.ToDoc

import System.Random ( randomRIO )
import Data.Ix
import Data.Maybe
import Control.Monad ( guard )

generator conf key = do
        s <- randomRIO $ C.start_size_range conf
        t <- Lambda.Roll.application s $ C.free_variables conf
        let ds = do
                d <- take 1000 $ derivations conf t
                guard $ I.steps d == Just ( C.derivation_length conf )
                guard $ nontrivial d
                return d
        case ds of
            [] -> generator conf key
            d : _ -> return d

nontrivial inst = isNothing $ result $ do
    let leftmost_strategy = replicate ( fromJust $ I.steps inst ) 0
    result <- derive ( I.from inst ) leftmost_strategy
    assert ( result == I.to inst ) $ text "OK"
    
derivations :: C.Type -> Lambda -> [ I.Type ]
derivations conf t =
    bfs  ( next conf ) ( I.initial t )

-- | all ways to extend given derivation by one step (at the end)
-- derivation must fulfil restrictions 
next :: C.Type -> I.Type -> Set I.Type
next conf inst = mkSet $ do
    let t = I.to inst
    p <- redex_positions t
    redex  <- peek t p
    redukt <- step redex
    result <- poke t ( p, redukt )
    guard $ inRange ( C.overall_size_range conf ) ( size result )
    l :: Int <- maybeToList $ I.steps inst
    guard $ inRange ( 0, C.derivation_length conf ) l
    return $ inst 
        { I.to = result 
        , I.steps = fmap succ $ I.steps inst
        }

