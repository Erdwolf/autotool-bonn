module NPDA.Inter where

--   $Id$

import NPDA.Type
import NPDA.Machine
import NPDA.Beispiel

import NPDA.Config
import NPDA.Property

import qualified Machine.Acceptor.Type as A
import qualified Machine.Acceptor.Inter

import Language.Type
import Language.Inter

import Autolib.Reporter
import qualified Autolib.Reporter.Checker as C
import Autolib.ToDoc
import Autolib.Informed

import Autolib.Util.Zufall


import Inter.Types
import Inter.Quiz

type Accept = A.Type ( NPDA Char Char Int ) String Property

instance Project ( A.Acceptor ) Accept Accept where 
    project _ i = i

instance Generator ( A.Acceptor ) Config Accept where
    generator _ config key = do
        let l = inter $ lang config
    	    m = max_num config
    	    e = max_length config
            small = \ w -> length w <= e
        yeah <- lift $ samples      l m 0
        noh  <- lift $ anti_samples l m 0
        return $ A.Make
    	   { A.machine_desc = text "Keller-Automat"
    	   , A.data_desc = info $ lang config
    	   , A.yeah = take m $ filter small yeah
    	   , A.noh  = take m $ filter small noh
    	   , A.cut  = cut config 
    	   , A.properties = Sane : properties config
    	   , A.start = start config
    	   }

make :: Make
make = quiz ( A.Acceptor "NPDA" ) NPDA.Config.example

