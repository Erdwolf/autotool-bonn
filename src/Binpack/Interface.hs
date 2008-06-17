{-# language FlexibleInstances, MultiParamTypeClasses #-}

module Binpack.Interface where

import Binpack.Instance
import Binpack.Example

import Autolib.FiniteMap
import Autolib.Set

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter

import Inter.Types
import Inter.Quiz

import Data.List ( sort )
import LCS.Code ( is_embedded_in )

instance Partial Binpack Instance [( Integer, Integer )] where

    describe _ i = toDoc i

    initial _ i = zip ( weights i )
                $ concat $ repeat [ 1 .. bins i ]

    partial _ i b = do

        let bad = do 
              (x,c) <- b
              guard $ c < 1 || bins i < c
        when ( not $ null bad ) $ reject $ vcat
             [ text "nicht die richtigen Behälter benutzt:"
             , toDoc bad
             ]

        sequence_ $ do 
            c <- [ 1 .. bins i ]
            return $ do
                let ws = map fst $ filter ( (== c) . snd ) b
                    t = sum ws
                inform $ vcat 
                       [ text "Behälter" <+> toDoc c
                       , nest 4 $ vcat
                                [ text "Gegenstände:" <+> toDoc ws
                                , text "Summe:" <+> toDoc t
                                ]
                       ]
                when ( t > capacity i ) $ reject $ text "zu groß"

    total _ i b = do

        let todo = sort $ weights i
            done = sort $ map fst b
        when ( todo /= done ) $ reject $ vcat
             [ text "nicht alle oder nicht die richtigen Gewichte verpackt:"
             , text "gegeben waren :" <+> toDoc todo
             , text "Sie benutzen  :" <+> toDoc done
             ]

instance Verify Binpack Instance where
    verify _ i = do
        let small = filter (<= 0) $ weights i
        when ( not $ null small ) $ reject $ vcat
             [ text "Gewichte nicht positiv:"
             , toDoc small
             ]
        let large = filter ( > capacity i ) $ weights i
        when ( not $ null large ) $ reject $ vcat
             [ text "Gewichte größer als Kapazität:"
             , toDoc large
             ]
        let totalweight = sum $ weights i
            totalcap = bins i * capacity i
        inform $ vcat 
               [ text "Gesamtgewicht  " <+> toDoc totalweight
               , text "Gesamtkapazität" <+> toDoc totalcap
               ]
        when ( totalweight > totalcap ) $ reject $ text "paßt nicht"

used_bins :: [(Integer,Integer)] -> Int
used_bins b = cardinality $ mkSet $ map snd b

instance Measure Binpack Instance [(Integer,Integer)] where
    measure _ i b = fromIntegral $ used_bins b
             
make_fixed :: Make
make_fixed = direct Binpack Binpack.Example.e1

