{-# language MultiParamTypeClasses, DeriveDataTypeable #-}

module Program.List.Instance where

import Program.List.Expression as X
import Program.List.Value as V
import Program.List.Semantics as S
import qualified Program.List.Config as F
import qualified Program.List.Roll as R

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


data Program_List = Program_List deriving ( Eq, Ord, Show, Read, Typeable )

instance OrderScore Program_List where
    scoringOrder _ = None -- ?

instance Class Program_List Statement V.Value where
    execute p = S.execute
    example p = ( read "x.add(1,y.get(2));"
                , read "List<int> x = { 1,2,3 } ; List<int> y = { 4,5,6 } ;"
                )

make_quiz :: Make
make_quiz = quiz Program_List F.example

instance Generator 
	     Program_List 
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
    isJust $ result $ C.total Program_List ( prog, final ) start



instance Project
	     Program_List  
	     ( Environment Program.Array.Value.Value
             , Program Statement 
             , Environment Program.Array.Value.Value 
             )
	     ( Program Statement
             , Environment Program.Array.Value.Value 
             ) where
    project _ ( start, p, final ) = ( p, final )



