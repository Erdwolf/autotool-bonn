{-# language OverlappingInstances, IncoherentInstances, FlexibleInstances #-}
{-# language MultiParamTypeClasses, DeriveDataTypeable #-}

module Graph.TSP.Plain where

import qualified Graph.Weighted  as W
import qualified Autolib.Graph.Graph as G
-- import Graph.Util
import Autolib.Dot ( peng, Layout_Program (..) )

import Graph.TSP.Tropic

import Graph.TSP.Search ( search )

import qualified Autolib.Reporter.Set ( eq , subeq )

import Inter.Types
import Data.Typeable ( Typeable )
import qualified Challenger as C

import Data.Maybe ( fromMaybe )
import Data.List ( transpose )
import qualified Data.Map as M
import qualified Data.Set as S

import Autolib.Reporter
import Autolib.ToDoc

-------------------------------------------------------------------------------

data TSP = TSP deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore TSP where
    scoringOrder _ = None

matrix :: W.Graph Int Int -> [[Tropic Int]]
matrix wg = do
    let (g , w) = W.extract wg
        v = S.toList $ G.knoten g
        bnd = ((1,1),(length v, length v))
    (i,x) <- zip [1..] v
    return $ do 
        (j,y) <- zip [1..] v
        return $ case M.lookup ( G.kante x y ) w of
            Nothing -> Infinite
            Just v -> Finite v

instance C.Partial TSP ( Tropic Int , W.Graph Int Int ) [ Int ]  where

    report TSP (b, wg) = do

        inform $ vcat 
	     [ text "Gesucht ist eine Rundreise mit Gewicht <=" <+> toDoc b
             , text "im Graphen mit der Distanz-Matrix"
             , nest 4 $ besides $ map ( vcat . map toDoc ) $ transpose $ matrix wg
             , text "Gewicht '+' bedeutet 'plus unendlich' (keine Kante)."
	     ]
        peng wg


    initial TSP (b, wg) = 
        S.toList $ W.knoten wg

    partial TSP (b, wg) p = do
        let duplicates = M.keys
                       $ M.filter (> 1) 
                       $ M.fromListWith (+) 
                       $ do x <- p ; return (x, 1)
        when ( not $ null duplicates ) 
             $ reject $ text "diese Knoten kommen mehrfach vor:" </> toDoc duplicates

        Autolib.Reporter.Set.eq 
            ( text "Knoten des Graphen", W.knoten wg )
            ( text "Knoten Ihrer Rundreise", S.fromList p )
 
    
    total TSP (b, wg) p = do
        let (g,w) = W.extract wg
            f x y = case M.lookup (G.kante x y) w of 
                 Nothing -> Infinite 
                 Just v  -> Finite v
        inform $ text "Die Kosten Ihrer Rundreise sind:" 
        cs <- nested 4 $ forM ( zip p $ rotate 1 p ) $ \ (x,y) -> do
              let c = f x y
              inform $ toDoc ( G.kante x y, c )
              return c
        let c = sum cs
        inform $ text "Summe:" </> toDoc c
        when ( c > b ) $ reject $ text "Das übersteigt die Schranke."


rotate :: Int -> [a] -> [a]
rotate k xs = 
    let ( pre, post ) = splitAt k xs
    in  post ++ pre

-------------------------------------------------------------------------------

make :: Make
make = direct TSP ( Finite 4 :: Tropic Int , W.example 7 )

