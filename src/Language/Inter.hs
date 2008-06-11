module Language.Inter where

--  $Id$

import Language.Syntax 
import Language.Type

import  Language.ABCdiff
import  Language.Gleich
import  Language.AK

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

import Language.RX
import Language.Grammatik

import Language.Center

inter :: Language.Syntax.Type 
      -> Language
inter l = case l of
    Form vs -> form vs
    ABCdiff -> abcdiff
    Gleich vs cs -> gleich vs cs
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
    Pali sigma -> pali sigma
    NoPali sigma -> nopali sigma
--    Potenzen i -> potenzen i
    Power vs i -> power vs i
    NoPower vs i -> nopower vs i
    Reg     sigma rx -> regular sigma $ read rx
    Regular sigma rx -> regular sigma $ read rx
    From_Grammatik g -> grammatik g
    Center sigma c -> center sigma c
--    Vielfache i -> vielfache i

    Uneps l -> uneps $ inter l 
    Komplement l -> komplement $ inter l 
    Mirror l -> mirror $ inter l 

