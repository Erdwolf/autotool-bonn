module NPDA.Accept_by 

where

import NPDA.Type
import NPDA.Property

import Control.Monad (guard)
import Autolib.Reporter
import qualified Autolib.Reporter.Checker  as C
import Autolib.Set
import Autolib.ToDoc

check :: NPDAC x y z => Acceptance_Mode -> C.Type ( NPDA x y z )
check m = C.Make 
      { C.nametag = unwords [ "accept_by" , show m ]
      , C.condition = case m of
             Empty_Stack -> text "Der Kellerautomat soll durch leeren Keller akzeptieren."
             Final_States -> text "Der Kellerautomat soll durch Finalzustände akzeptieren."
      , C.investigate = accept_by m
      }

accept_by 
    :: Acceptance_Mode 
    -> NPDAC x y z
    => NPDA x y z -> Reporter ()
accept_by m a = 
    case ( m, akzeptiert a ) of
         ( Empty_Stack, Leerer_Keller ) -> 
             inform $ text "ok"
         ( Final_States, Zustand _ ) ->
             inform $ text "ok"
         _ -> reject $ text "nicht ok"


