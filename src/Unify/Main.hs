{-# language DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies, TypeSynonymInstances #-}


module Unify.Main where

import Unify.Instance ( Instance, InstanceC )
import qualified Unify.Instance as I
import Unify.Config ( Config )
import qualified Unify.Config as C
import Unify.Roll

import Autolib.ToDoc
import Autolib.Reader
import Autolib.Reporter
import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Autolib.TES.Identifier
import Autolib.Symbol
import Autolib.TES.Term
import Autolib.TES.Unify
import Autolib.TES.Type
import Autolib.TES.Position
import Autolib.TES.Apply
import Autolib.Size
import Autolib.FiniteMap

import Data.Typeable

data Unify = Unify deriving ( Eq, Ord, Show, Read, Typeable )


instance Measure Unify ( Instance v c ) ( Term v c, Term v c ) where
    measure p i ( t1, t2 ) = fromIntegral $ size t1 + size t2

instance InstanceC v c => Partial Unify ( Instance v c ) ( Term v c, Term v c ) where
    describe p i = I.describe i

    initial p i = ( I.left i, I.right i )

    partial p i ( t1, t2 ) = do
        conforms ( I.wildcard i ) ( I.left i ) t1
        conforms ( I.wildcard i ) ( I.right i ) t2

    total p i ( t1, t2 ) = do
        let t1s = flip apply_partial t1 $ I.unifier i
            t2s = flip apply_partial t2 $ I.unifier i
        inform $ vcat
            [ text "die Substitution erzeugt"
            , nest 4 $ toDoc ( t1s, t2s )
            ]
        when ( t1s /= t2s ) $ reject $ text "Diese Terme stimmen nicht überein."

        case mgu t1 t2 of
            Just u  -> do
                inform $ vcat 
                       [ text "ein allgemeinster Unifikator Ihrer Terme ist" 
                       , nest 4 $ toDoc u 
                       ]
                let size fm = sum $ do ( v, t ) <- fmToList fm ; return $ Autolib.Size.size t
                assert ( size u == size ( I.unifier i ) ) $ vcat 
                       [ text "vereinfachter Test: stimmen die Summen der Größen der Bilder"
                       , text "für diesen Unifikator und den aus der Aufgabenstellung überein?"
                       ]


conforms w p t = do
    inform $ vcat 
           [ hsep [ text "paßt der Term" <+> toDoc t ]
           , hsep [ text "zum Muster" <+> toDoc p ]
           , hsep [ text "?" ]
           ]
    let check p t = case ( p, t ) of
            ( Node f _ , _ ) | f == w -> return ()
            ( Node f xs , Node g ys ) | f == g -> 
                mapM_ ( uncurry check ) $ zip xs ys
            ( _ , _ ) -> when ( p /= t ) $ reject 
                    $ hsep [ text "Nein:", toDoc p, text "/=", toDoc t ]
    check p t
    inform $ text "Ja."


make_fixed :: Make
make_fixed = direct Unify I.example


instance InstanceC v c => Generator Unify ( Config v c ) ( Instance v c ) where
    generator p conf key = roll conf

instance InstanceC v c => Project  Unify ( Instance v c ) ( Instance v c ) where
    project p i = i


{-
type CII = Config Identifier Identifier
type III = Instance Identifier Identifier

instance Generator Unify CII III where
    generator p conf key = roll conf

instance Project  Unify III III where
    project p i = i
-}

make_quiz :: Make
make_quiz = quiz Unify C.example


