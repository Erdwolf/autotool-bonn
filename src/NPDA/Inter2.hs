module NPDA.Inter2 where

--   $Id$

import NPDA.Type
import NPDA.Machine
import NPDA.Beispiel

import NPDA.Config2
import NPDA.Property

import qualified Machine.Acceptor.Type2 as A
import qualified Machine.Acceptor.Inter2

import Inter.Types

type Accept = A.Type ( NPDA Char Char Int ) String Property


make :: Make
make = direct ( A.Acceptor "NPDA" ) 
         ( NPDA.Config2.example )

