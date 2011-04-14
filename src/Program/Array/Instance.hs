{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable #-}

module Program.Array.Instance where

import Program.Array.Statement
import Program.Array.Value
import Program.Array.Semantics

import qualified Program.Array.Roll as R
import qualified Program.Array.Config as F

import Program.General.Class
import Program.General.Central
import Program.General.Environment
import Program.General.Program

import Autolib.Reader
import Autolib.ToDoc
import Autolib.Reporter

import qualified Challenger as C

import Inter.Types
import Inter.Quiz
import Autolib.Size
import Autolib.Util.Zufall ( repeat_until )

import Data.Typeable
import Data.Maybe ( isNothing, isJust )


data Program_Array = Program_Array deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Program_Array where
    scoringOrder _ = None -- ?

instance Class Program_Array Statement Program.Array.Value.Value where
    execute p = Program.Array.Semantics.execute
    example p = ( Program [ Program.Array.Statement.s0 ]
                , Program.General.Environment.make 
                      [ ( read "x", Program.Array.Value.example ) ]
                )

make_quiz :: Make
make_quiz = quiz Program_Array F.example


instance Generator 
	     Program_Array 
	     F.Config 
	     ( Environment Program.Array.Value.Value
             , Program Statement 
             , Environment Program.Array.Value.Value 
             ) where
    generator p conf key = 
        R.roll conf `repeat_until` nontrivial conf

nontrivial conf (_, Program sts , final) = not $ or $ do
    let bnd = ( 0 , fromIntegral $ F.max_data_size conf )
    ps <- [] : map return ( patches final bnd )
    return $ matches ( final ,  Program $ ps ++ sts , final )

matches ( start, prog, final ) = 
    isJust $ result $ C.total Program_Array ( prog, final ) start



instance Project
	     Program_Array 
	     ( Environment Program.Array.Value.Value
             , Program Statement 
             , Environment Program.Array.Value.Value 
             )
	     ( Program Statement
             , Environment Program.Array.Value.Value 
             ) where
    project _ ( start, p, final ) = ( p, final )

