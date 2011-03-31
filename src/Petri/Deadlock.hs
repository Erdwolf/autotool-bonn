{-# language DeriveDataTypeable, TemplateHaskell #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleInstances #-}

module Petri.Deadlock where

import Petri.Type
import Petri.Step
import Petri.Roll

import Autolib.Reader
import Autolib.ToDoc
import Data.Typeable
import Data.List ( minimumBy, maximumBy )
import Data.Ord ( comparing )
import qualified Data.Set as S
import qualified Data.Map as M

import Challenger.Partial
import Autolib.Reporter 
import Autolib.Size
import Inter.Types
import Inter.Quiz

data Petri_Deadlock = Petri_Deadlock 

$(derives [makeReader, makeToDoc] [''Petri_Deadlock]) 
                                        
instance Partial Petri_Deadlock 
         ( Net Place Transition )
         [ Transition ] where
    describe _ n = vcat
         [ text "Gesucht ist für das Petri-Netz"
         , nest 4 $ toDoc n  
         , text "eine Transitionsfolge," 
         , text "die zu einem Zustand ohne Nachfolger (Deadlock) führt."
         ]  
    initial _ n = 
        reverse $ S.toList $ transitions n     
    total _ n ts = do
        out <- foldM ( \ z (k, t) -> do
                          inform $ text "Schritt" <+> toDoc k
                          Petri.Step.execute n t z ) 
                     ( start n ) ( zip [ 1 :: Int .. ] ts )
        assert ( null $ successors n out )
               $ text "Zielzustand hat keine Nachfolger?"

data Config = Config 
                 { num_places :: Int
                 , num_transitions :: Int  
                 , capacity :: Capacity Place
                 , max_transition_length :: Int  
                 }  deriving Typeable
                             
example :: Config
example = Config
    { num_places = 4
    , num_transitions = 4
    , Petri.Deadlock.capacity =  Unbounded 
                             -- All_Bounded 1
    , max_transition_length = 10
    }
                          
$(derives [makeReader, makeToDoc] [''Config]) 
            
instance Generator 
    Petri_Deadlock Config 
      ( Int, Net Place Transition ) where
      generator _ conf key = do
          tries <- forM [ 1 .. 1000 ] $ \ k -> do
              let ps = [ Place 1 
                         .. Place ( num_places conf ) ]
                  ts = [ Transition 1 
                         .. Transition ( num_transitions conf ) ]
              n <- Petri.Roll.net ps ts 
                   ( Petri.Deadlock.capacity conf ) 
              return $ do
                    let ( no, yeah ) = span ( null . snd )
                                     $ take ( max_transition_length conf )
                                     $ zip [ 0..]
                                     $  deadlocks n
                    guard $ not $ null yeah
                    return ( length no, n )
          return $ maximumBy ( comparing fst ) 
                 $ concat tries
  
  