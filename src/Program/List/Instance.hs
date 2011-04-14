{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable, FlexibleInstances #-}

module Program.List.Instance where

import Program.List.Expression as X
import Program.List.Value as V
import Program.List.Semantics as S
import qualified Program.List.Operation as O
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
                , read "List<int> x = { 1,5,3 } ; List<int> y = { 4,5,6 } ;"
                )

make_quiz :: Make
make_quiz = quiz Program_List F.example

instance Generator 
	     Program_List 
	     F.Config 
	     ( Environment V.Value
             , Program Statement 
             , Maybe ( Environment V.Value )
             ) where
    generator p conf key = do
            e <- R.environment conf
            p <- R.program O.ops e conf 
            let mf = result $ Program.General.Class.execute Program_List e p
            return ( e, p, mf )
        `repeat_until` \ (e,p,mf) -> isJust mf 
             && size p > 3 * plength p
             && Just e /= mf

instance Project
	     Program_List  
	     ( Environment V.Value
             , Program Statement 
             , Maybe ( Environment V.Value  )
             )
	     ( Program Statement
             , Environment V.Value 
             ) where
    project _ ( start, p, Just final ) = ( p, final )



