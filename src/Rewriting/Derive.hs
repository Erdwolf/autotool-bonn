{-# language UndecidableInstances #-}
-- | this is the general module (top module after refactoring)

module Rewriting.Derive where

import Rewriting.Apply
import Rewriting.Derive.Instance

{-
import Rewriting.Step
import Rewriting.Steps
import Rewriting.Derive.Quiz
import Rewriting.Derive.Config
-}

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.Reader
import Autolib.FiniteMap

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Control.Monad
import Data.Typeable


data Derive tag = Derive tag 
    deriving ( Eq, Ord, Typeable )

instance OrderScore ( Derive tag ) where
    scoringOrder _ = Increasing

instance ToDoc tag => ToDoc ( Derive tag ) where
    toDoc ( Derive t ) = text "Derive-" <> toDoc t

instance Reader tag => Reader ( Derive tag ) where
    reader = do 
        my_symbol "Derive-"
        tag <- reader
        return $ Derive tag

class ( Reader x, ToDoc x, Typeable x ) => RDT x
instance ( Reader x, ToDoc x, Typeable x ) => RDT x

instance ( RDT tag, RDT action, RDT object , RDT system
         , Eq object
         , Apply tag system object action 
         )
    => Partial ( Derive tag ) ( Instance system object ) [ action ] where

    report ( Derive tag ) inst = do 
        inform $ vcat
            [ text "gesucht ist für das System"
            , nest 4 $ toDoc $ system inst
            , text "eine Folge von Schritten, die"
            , nest 4 $ toDoc $ from inst
            , text "überführt in"
            , nest 4 $ toDoc $ to inst
            ]
        -- peng $ from inst
        -- peng $ to inst
        

    initial ( Derive tag ) inst = 
        take 1 $ actions tag ( system inst ) ( from inst ) 

    total ( Derive tag ) inst steps = do
        let sys = system inst
        t <- foldM ( apply tag sys ) ( from inst ) steps
        assert ( t == to inst )
               $ text "stimmt mit Ziel überein?"

instance Measure ( Derive tag ) ( Instance system object ) [ action ] where
    measure ( Derive tag ) inst actions = fromIntegral $ length actions


make_fixed :: ( RDT tag, RDT action, RDT object , RDT system
         , Eq object
         , Apply tag system object action 
         )
           => tag -> Make
make_fixed tag = direct ( Derive tag ) ( example tag )


{-

make_quiz :: Make
make_quiz = quiz Derive Rewriting.Derive.Config.example


instance (Symbol v, Symbol c, Reader ( TRS v c ) )
    => Generator Derive ( Config v c ) ( Instance v c ) where
    generator Derive conf key = roll conf

instance (Symbol v, Symbol c, Reader ( TRS v c ) )
    => Project  Derive ( Instance v c ) ( Instance v c ) where
    project  Derive inst = inst

firstvar trs = take 1 $ do
    r <- regeln trs
    t <- [ lhs r, rhs r ]
    lvars t
-}

