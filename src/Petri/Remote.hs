{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable #-} 
{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-} 

module Petri.Remote where

import Petri.Type
import Petri.Step
import Petri.Roll
import Petri.Dot
import Petri.Property

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Dot.Dotty ( peng )
import Data.Typeable
import Data.List ( minimumBy, maximumBy )
import Data.Ord ( comparing )
import qualified Data.Set as S
import qualified Data.Map as M

import Challenger.Partial
import Autolib.Reporter 
import Autolib.Size
import Autolib.Hash
import Inter.Types
import Inter.Quiz

data Petri_Deadlock_Remote = Petri_Deadlock_Remote
    deriving ( Typeable )

instance OrderScore Petri_Deadlock_Remote where
    scoringOrder _ = Decreasing

$(derives [makeReader, makeToDoc] [''Petri_Deadlock_Remote]) 
                                        
instance ( Ord s, Ord t, Hash s, Hash t, Size t
         , ToDoc s, ToDoc t , Reader s, Reader t
         ) =>
       Measure Petri_Deadlock_Remote [ Property] ( Net s t, [ t ] ) where
    measure _ props ( n, ts ) = fromIntegral $ length ts

instance Partial Petri_Deadlock_Remote [ Property ] 
         ( Net Place Transition, [ Transition ] ) where
    report _ props = do
         inform $ vcat
             [ text "Gesucht ist ein Petri-Netz mit den Eigenschaften"
               </> toDoc props             
             , text "für das die kürzeste Schaltfolge zu einem Deadlock"
             , text "möglichst lang ist."
             , text "Eine solche Schaltfolge ist ebenfalls anzugeben."
             ]
    initial _ props = 
            ( fst Petri.Type.example 
            , S.toList $ transitions $ fst Petri.Type.example
            )
    partial _ props ( n, ts ) = do
        validates ( Default : props ) n
        inform $ text "Netz mit Startzustand:"
        peng n    
        out <- silent $ executes n ts
        inform $ vcat
               [ text "Nach Ausführung von" <+> toDoc ts
               , text "wird dieser Zustand erreicht:" </> toDoc out
               ]
        peng $ n { start = out }
        assert ( null $ successors n out )
               $ text "dieser Zustand hat keine Nachfolger?"

    total _ props ( n, ts ) = do
        let infos = do 
                 ( k, ds ) <- zip [ 0 .. length ts - 1 ] $ deadlocks n
                 guard $ not $ null ds
                 return $ fsep 
                     [ text "Weglänge:", toDoc k
                     , text "Anzahl toter Zustände:", toDoc ( length ds )
                     ]
        if ( null infos )
           then inform $ text "Es gibt keinen kürzeren Weg zu einem Deadlock."
           else reject $ text "Es gibt kürzere Wege zu Deadlocks:" </>
               vcat ( take 2 infos )

make_fixed :: Make
make_fixed = direct Petri_Deadlock_Remote 
    [ Max_Num_Places 5
    , Max_Initial_Tokens 1
    , Max_Edge_Multiplicity 1
    , Capacity Unbounded
    ]