{-# language DeriveDataTypeable, TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module Petri.Reach where

import Petri.Type
import Petri.Step
import Petri.Roll
import Petri.Dot
import Petri.Property

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Dot.Dotty ( peng )
import Data.Typeable
import Data.List ( minimumBy )
import Data.Ord ( comparing )
import qualified Data.Set as S
import qualified Data.Map as M

import Challenger.Partial
import Autolib.Reporter 
import Autolib.Size
import Inter.Types
import Inter.Quiz

data Petri_Reach = Petri_Reach 
    deriving ( Typeable )

instance OrderScore Petri_Reach where
    scoringOrder _ = Increasing

$(derives [makeReader, makeToDoc] [''Petri_Reach]) 

instance Verify Petri_Reach 
         ( Net Place Transition, State Place ) where
    verify Petri_Reach ( n, s ) = do
        validate Default $ n
        validate Default $ n { start =s }
                                        
instance Partial Petri_Reach 
         ( Net Place Transition, State Place ) 
         [ Transition ] where
    report _ ( n, goal ) = do
         inform $ vcat
             [ text "Gesucht ist für das Petri-Netz"
             , nest 4 $ toDoc n  
             ]
         peng n
         inform $ vcat
             [ text "eine Transitionsfolge," 
             , text "durch die der folgende Zustand erreicht wird:"
             , nest 4 $ toDoc goal  
             ]  
    initial _ ( n, goal ) = 
        reverse $ S.toList $ transitions n     
    total _ ( n, goal ) ts = do
        inform $ text "Startzustand" </> toDoc ( start n )
        out <- foldM ( \ z (k, t) -> do
                          inform $ text "Schritt" <+> toDoc k
                          Petri.Step.execute n t z ) 
                     ( start n ) ( zip [ 1 :: Int .. ] ts )
        assert ( out == goal ) 
               $ text "Zielzustand erreicht?"

make_fixed :: Make
make_fixed = direct Petri_Reach Petri.Type.example

data Config = Config 
                 { num_places :: Int
                 , num_transitions :: Int  
                 , capacity :: Capacity Place
                 , transition_length :: Int  
                 }  deriving Typeable
                             
example :: Config
example = Config
    { num_places = 4
    , num_transitions = 4
    , Petri.Reach.capacity = -- Unbounded 
                             All_Bounded 1
    , transition_length = 8
    }
                          
$(derives [makeReader, makeToDoc] [''Config]) 


make_quiz :: Make
make_quiz = quiz Petri_Reach Petri.Reach.example
            
instance Project 
    Petri_Reach 
      ( ( Net Place Transition, State Place ) )
      ( ( Net Place Transition, State Place ) )  where
    project _ nz = nz

instance Generator 
    Petri_Reach Config 
      ( ( Net Place Transition, State Place ) ) where
      generator _ conf key = do
          tries <- forM [ 1 .. 1000 ] $ \ k -> do
              let ps = [ Place 1 
                         .. Place ( num_places conf ) ]
                  ts = [ Transition 1 
                         .. Transition ( num_transitions conf ) ]
              n <- Petri.Roll.net ps ts ( Petri.Reach.capacity conf ) 
              case drop ( transition_length conf ) ( levels n ) of
                  [] -> return []
                  zs : _ -> return $ do 
                      z' <- zs
                      let d = sum $ do
                            p <- ps
                            return $ abs ( mark ( start n ) p - mark z' p )
                      return ( d, ( n, z' ) ) 
          return $ snd
                 $  minimumBy ( comparing fst ) 
                 $ concat tries
  
  