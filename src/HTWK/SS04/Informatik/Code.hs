module HTWK.SS04.Informatik.Code where

--  $Id$

import qualified Code.Huffman.Quiz as H
import qualified Code.Quiz as C
import qualified Code.Move_To_Front 
import qualified Code.Burrows_Wheeler

import Sets
import Inter.Types

generates :: [ IO Variant ]
generates = 
    [ H.make $ H.Config 
	     { H.alphabet = mkSet [ 'a' .. 'i' ]
	     , H.range    = ( 5, 15 )
	     }
    , C.enc  $ C.Config
	     { C.coder    = Code.Move_To_Front.coder
	     , C.alphabet = mkSet [ 'a' .. 'e' ]
	     , C.length_range = ( 10, 15 )
	     }
    , C.dec  $ C.Config
	     { C.coder    = Code.Burrows_Wheeler.coder
	     , C.alphabet = mkSet [ 'a' .. 'f' ]
	     , C.length_range = ( 12, 15 )
	     }
    ]

 
     