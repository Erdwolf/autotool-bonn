{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-incoherent-instances #-}

module Rewriting.Derive where

import Rewriting.TRS
import Type.Tree

import Rewriting.Derive.Instance
import Rewriting.Step
import Rewriting.Steps
import Rewriting.Derive.Quiz
import Rewriting.Derive.Config

import Autolib.Reporter
import Autolib.ToDoc
import Autolib.FiniteMap

import Challenger.Partial
import Inter.Types
import Inter.Quiz

import Control.Monad
import Data.Typeable

data Derive = Derive 
    deriving ( Eq, Ord, Show, Typeable )

instance (  Symbol c )
    => Partial Derive ( Instance c c ) [ Step c c ] where

    report Derive inst = do 
        inform $ vcat
            [ text "gesucht ist für das Term-Ersetzungs-System"
            , nest 4 $ toDoc $ system inst
            , text "eine Folge von Schritten, die"
            , nest 4 $ toDoc $ from inst
            , text "überführt in"
            , nest 4 $ toDoc $ to inst
            ]
        peng $ from inst
        peng $ to inst
        

    initial Derive inst = 
        [ Step { rule_number = 2
               , position = [0,2]
               , substitution = listToFM 
                              $ zip ( variablen $ system inst )
                              $ drop 2 $ subterms $ from inst
               }
        ]

    total Derive inst steps = do
        sys <- patch $ system inst
        t <- foldM ( exec sys ) ( from inst ) steps
        assert ( t == to inst )
               $ text "stimmt mit Ziel überein?"

-- | upon reading, the parser does not know what is a variable
-- so the system has to be patched
patch :: ( Symbol c ) 
      => TRS c c -> Reporter ( TRS c c )
patch trs = do
    let handle t @ ( Node f xs ) =
           if f `elem` variablen trs 
              then if null xs 
                   then return $ Rewriting.TRS.Var f
                   else reject $ text "Variable darf keine Argumente haben:" 
                               <+> toDoc t
              else do
                   ys <- mapM handle xs
                   return $ Node f ys
    rules <- sequence $ do
        rule <- regeln trs
        return $ do
            ll <- handle $ lhs rule
            rr <- handle $ rhs rule
            return $ rule { lhs = ll, rhs = rr }
    return $ trs { regeln = rules }   

make_fixed :: Make
make_fixed = direct Derive Rewriting.Derive.Instance.example

make_quiz :: Make
make_quiz = quiz Derive Rewriting.Derive.Config.example

instance (Symbol v, Symbol c)
    => Generator Derive ( Config v c ) ( Instance v c ) where
    generator Derive conf key = roll conf

instance (Symbol v, Symbol c)
    => Project  Derive ( Instance v c ) ( Instance v c ) where
    project  Derive inst = inst

firstvar trs = take 1 $ do
    r <- regeln trs
    t <- [ lhs r, rhs r ]
    lvars t