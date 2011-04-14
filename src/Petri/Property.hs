{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable #-} 
{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-} 

module Petri.Property where

import Petri.Type

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader

import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad ( forM, void )
import Data.Typeable

data Property = Default
              | Max_Num_Places Int
              | Max_Num_Transitions Int
              | Max_Edge_Multiplicity Int
              | Max_Initial_Tokens Int
              | Capacity ( Capacity () )
     deriving ( Typeable )

$(derives [makeReader, makeToDoc] [''Property])
              
validates props n = void $ forM props $ \ prop -> validate prop n

validate :: ( Ord s, Ord t, ToDoc s, ToDoc t )
         => Property 
         -> Net s t 
         -> Reporter ()
validate p n = case p of
    Max_Num_Places m -> guard_bound ( text "Anzahl der Stellen" ) 
                    ( S.size $ places n ) m
    Max_Num_Transitions m -> guard_bound 
        ( text "Anzahl der Transitionen" ) ( S.size $ transitions n ) m
    Max_Initial_Tokens m -> guard_bound 
        ( text "Anzahl der Token in Startzustand" )
                    ( sum $ M.elems $ unState $ start n ) m
    Max_Edge_Multiplicity m -> void $
        forM ( connections n ) $ \ c @ (vor, t, nach) -> do
            let bad_vor = M.filter ( > m ) 
                    $ M.fromListWith (+) $ zip vor $ repeat 1
                bad_nach = M.filter ( > m ) 
                    $ M.fromListWith (+) $ zip nach $ repeat 1
            when ( not $ M.null bad_vor ) $ reject $ vcat
                [ text "Verbindung:" <+> toDoc c
                , text "Vielfachheit der Eingangskanten zu hoch:"
                    </> toDoc bad_vor
                ]
            when ( not $ M.null bad_nach ) $ reject $ vcat
                [ text "Verbindung:" <+> toDoc c
                , text "Vielfachheit der Ausgangskanten zu hoch:"
                    </> toDoc bad_nach
                ]

    Capacity cap -> case cap of
        Unbounded -> when ( capacity n /= Unbounded) $ reject
            $ text "Als Kapazität ist vorgeschrieben:" <+> toDoc cap
        All_Bounded b -> when ( capacity n /= All_Bounded b) $ reject
            $ text "Als Kapazität ist vorgeschrieben:" <+> toDoc cap

    Default -> do

        when ( not $ all_non_negative $ start n ) $ reject
             $ text "Startzustand enthält negative Markierungen"
        when ( not $ conforms ( capacity n ) ( start n ) ) $ reject
             $ text "Startzustand überschreitet Kapazitäten"

        forM ( connections n ) $ \ c @ ( vor, t, nach ) -> do
            when ( not $ S.member t $ transitions n ) $ reject $ vcat
                 [ text "Verbindung:"  <+> toDoc c
                 , text "nicht deklarierte Transition:" <+> toDoc t 
                 ]
            forM vor $ \ v -> do
                when ( not $ S.member v $ places n ) $ reject $ vcat
                 [ text "Verbindung:"  <+> toDoc c
                 , text "nicht deklarierte Stelle im Vorbereich:" <+> toDoc v
                 ]
            forM nach $ \ a -> do
                when ( not $ S.member a $ places n ) $ reject $ vcat
                 [ text "Verbindung:"  <+> toDoc c
                 , text "nicht deklarierte Stelle im Nachbereich:" <+> toDoc a
                 ]

        case capacity n of
            Bounded f -> do
                let out = S.difference ( M.keysSet f ) ( places n )
                when ( not $ S.null out ) $ reject $ vcat
                    [ text "nicht definierte Stellen in Kapazitätsfunktion:"
                    , toDoc out
                    ]
            _ -> return ()

        let out = S.difference ( M.keysSet $ unState $ start n ) ( places n )
        when ( not $ S.null out ) $ reject $ vcat
                    [ text "nicht definierte Stellen im Startzustand:"
                    , toDoc out
                    ]
        
guard_bound name actual bound =  
    when ( actual > bound) $ reject $ vcat
        [ name <+> parens ( toDoc actual )
        , text "ist größer als die Schranke" <+> parens ( toDoc bound )
        ]

