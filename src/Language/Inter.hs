module Language.Inter where

--  $Id$

import Language.Syntax 
import Language.Type

import  Language.ABCdiff
import  Language.Gleich
-- import  Language.AK

-- import  Language.AmBnCmDn
-- import  Language.BinCompare
-- import  Language.Cee

import  Language.IJK
-- import  Language.Kleiner
import  Language.Lukas

import  Language.Pali
-- import  Language.Potenzen
import  Language.Power
-- import  Language.Primitiv
-- import  Language.Pump_Counter
-- import  Language.Vielfache
-- import  Language.WWRW

inter :: Language.Syntax.Type 
      -> Language
inter l = case l of
    ABCdiff -> abcdiff
    Gleich vs -> gleich vs
    Ordered_Gleich vs -> ordered_gleich vs
    Ordered_Ungleich vs -> ordered_ungleich vs
--    Form vs -> form vs
--    AmBnCmDn -> ambncmdn
--    BinCompare -> bincompare
--    Cee c l -> cee c ( inter l )
    Lukas -> lukas
    NoLukas -> nolukas
    Dyck -> dyck
    NoDyck -> nodyck
    Pali -> pali
    NoPali -> nopali
--    Potenzen i -> potenzen i
    Power i -> power i
    NoPower i -> nopower i
--    Vielfache i -> vielfache i

