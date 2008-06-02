module NPDA.Check where

import NPDA.Type
import NPDA.Property
import NPDA.Det
import NPDA.Sane
import NPDA.Accept_by

import Condition
import qualified Autolib.Reporter.Checker as C

instance Condition  Property ( NPDA Char Char Int ) where
    condition p a = C.run ( NPDA.Check.check p ) a

instance Explain Property where
    explain p = C.condition ( NPDA.Check.check p )

check :: Property -> C.Type  ( NPDA Char Char Int )
check p = case p of
	 Sane -> NPDA.Sane.sanity
         Det -> NPDA.Det.check 
         Accept_by m -> NPDA.Accept_by.check m
