{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Binpack.Interface where

import Binpack.Instance
import Binpack.Example
import qualified Binpack.Param as P
import Binpack.Quiz
import Binpack.Approximation
import Binpack.InstanceTH ()

import Autolib.FiniteMap
import Autolib.Set

import Challenger.Partial
import Autolib.ToDoc
import Autolib.Reporter
import Autolib.Util.Zufall

import Inter.Types
import Inter.Quiz

import Data.List ( sort )
import LCS.Code ( is_embedded_in )

instance OrderScore Binpack where
    scoringOrder _ = Increasing

instance Partial Binpack Instance Assignment where

    describe _ i = toDoc i

    initial _ i = eltsFM $ addListToFM_C (++) emptyFM $ zip 
                ( concat $ repeat [ 1 .. bins i ] )
                ( map return $ weights i )

    partial _ i b = do

        when ( length b > bins i ) $ reject $ vcat
             [ text "zu viele Behälter benutzt"
             ]

        sequence_ $ do 
            bin <- b
            return $ do
                let t = sum bin
                inform $ vcat 
                       [ text "Behälter" 
                       , nest 4 $ vcat
                                [ text "Gegenstände:" <+> toDoc bin
                                , text "Summe:" <+> toDoc t
                                ]
                       ]
                when ( t > capacity i ) $ reject $ text "zu groß"

    total _ i b = do

        let todo = sort $ weights i
            done = sort $ concat b
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
            totalcap = fromIntegral ( bins i ) * capacity i
        inform $ vcat 
               [ text "Gesamtgewicht  " <+> toDoc totalweight
               , text "Gesamtkapazität" <+> toDoc totalcap
               ]
        when ( totalweight > totalcap ) $ reject $ text "paßt nicht"

used_bins :: Assignment -> Int
used_bins b = length b

instance Measure Binpack Instance Assignment where
    measure _ i b = fromIntegral $ used_bins b
             
make_fixed :: Make
make_fixed = direct Binpack Binpack.Example.e1


instance Generator Binpack P.Param Instance  where
    generator p conf key = generate_with_distance 1 conf 


generate_with_distance d conf = 
        do ws <- pick conf
           return $ Instance { capacity = P.capacity conf
                             , bins = P.bins conf
                             , weights = ws
                             }
        `repeat_until` \ i -> first_fit_decreasing_size i >= d + bins i
        

instance Project Binpack Instance Instance where
   project p i = i

make_quiz :: Make
make_quiz = quiz Binpack P.example





