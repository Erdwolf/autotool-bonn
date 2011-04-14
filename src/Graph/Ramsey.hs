{-# LANGUAGE UndecidableInstances, DeriveDataTypeable #-}

module Graph.Ramsey where

import Graph.Util

import Autolib.Graph.Ops ( complement )
import Autolib.Graph.Basic

import Inter.Types
import Autolib.ToDoc
import Autolib.Size
import Autolib.FiniteMap
import Autolib.Set
import qualified Challenger as C

import Data.Typeable
import Data.List
import Data.Maybe

data Ramsey = Ramsey deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Ramsey where
    scoringOrder _ = Decreasing

instance C.Verify Ramsey [Int] where
    verify p i = do
        when (  null i ) 
             $ reject 
             $ text "Die Liste der x_i darf nicht leer sein."
        when ( any ( <= 0 ) i ) 
             $ reject 
             $ text "Alle x_i sollen positiv sein."

instance C.Partial Ramsey [Int] ( Int, FiniteMap ( Kante Int ) Int ) where

    describe p i = vcat $
        [ text "Gesucht ist eine Kantenfärbung eines K_n"
	, hsep [ text "mit", toDoc ( length i ), text "Farben" ]
        , text "ohne einfarbige K_x_i der Farbe i"
        , hsep [ text "für [ x_1, .. ]", equals, toDoc i ]
	] 

    initial p i = 
        let n = sum i
        in  ( n
            , listToFM $ zip ( lkanten $ clique $ mkSet [ 1 .. n ] )
                       $ concat $ repeat [ 1 .. length i ]
            )

    partial p i (n, f) = do
        let missing = kanten ( clique $ mkSet [ 1 .. n ] ) 
                 `minusSet` mkSet ( keysFM f ) 
        when ( not $ isEmptySet missing ) $ reject $ vcat
             [ text "diese Kanten sind nicht gefärbt:"
             , nest 4 $ toDoc missing
             ]
        let fehlfarben = filter ( \ (k,f) -> not ( 1 <= f && f <= length i ) )
                       $ fmToList f
        when ( not $ null fehlfarben ) $ reject $ vcat
             [ text  "diese Kantenfarben sind nicht erlaubt:"
             , nest 4 $ toDoc fehlfarben
             ]
    
    total p i (n, f) = do
        sequence_ $ do 
            (c, x_c) <- zip [ 1.. ] i
            cl <- monochromatic_cliques n c f
            return $ when ( length cl >= x_c ) $ reject $ vcat
                        [ text "Der Teilgraph mit dieser Knotenmenge"
                        , nest 4 $ toDoc cl
                        , text "ist einfarbig mit Farbe" <+> toDoc c
                        ]

monochromatic_cliques n c f = 
    let find cand [] = [ reverse cand ]
        find cand xs = do
              (y : ys) <- tails xs
              guard $ and $ do x <- cand ; return $ Just c == lookupFM f ( kante x y )
              find (y : cand) ys
    in  find [] [ 1 .. n ]

instance C.Measure Ramsey [Int] ( Int , FiniteMap (Kante Int) Int ) where 
    measure p i ( n, f ) = fromIntegral n

make :: Make
make = direct Ramsey [ 4, 4 :: Int ]




