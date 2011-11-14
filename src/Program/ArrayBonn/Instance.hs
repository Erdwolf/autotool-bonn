{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, FlexibleInstances #-}

module Program.ArrayBonn.Instance where

import Program.ArrayBonn.Statement
import Program.ArrayBonn.Value
import Program.ArrayBonn.Semantics

import qualified Program.ArrayBonn.Roll as R
import qualified Program.ArrayBonn.Config as F

import Program.GeneralBonn.Class
import Program.GeneralBonn.Central
import Program.GeneralBonn.Environment
import Program.GeneralBonn.Program

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

instance Class Program_Array Statement Program.ArrayBonn.Value.Value where
    execute p = Program.ArrayBonn.Semantics.execute
    example p = ( Program [ Program.ArrayBonn.Statement.s0 ]
                , Program.GeneralBonn.Environment.make 
                      [ ( read "x", Program.ArrayBonn.Value.example ) ]
                )

make_quiz :: Make
make_quiz = quiz Program_Array F.example


instance Generator 
	     Program_Array 
	     F.Config 
	     ( Environment Program.ArrayBonn.Value.Value
             , Program Statement 
             , Environment Program.ArrayBonn.Value.Value 
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
	     ( Environment Program.ArrayBonn.Value.Value
             , Program Statement 
             , Environment Program.ArrayBonn.Value.Value 
             )
	     ( Program Statement
             , Environment Program.ArrayBonn.Value.Value 
             ) where
    project _ ( start, p, final ) = ( p, final )

