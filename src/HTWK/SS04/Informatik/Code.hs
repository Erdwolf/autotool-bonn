module HTWK.SS04.Informatik.Code where

--  $Id$

import qualified Code.Huffman.Quiz as H
import qualified Code.Quiz as C
import qualified Code.Move_To_Front 
import qualified Code.Burrows_Wheeler
import qualified Code.Hamming as A

import Sets
import Inter.Types

generates :: [ IO Variant ]
generates = 
    [ A.make $ A.Config
             { A.length   = (A.Atmost,  6)
	     , A.size     = (A.Atleast, 5)
	     , A.distance = (A.Atleast, 3)
	     , A.optimize = "Größe"
	     }
    , A.make $ A.Config
             { A.length = (A.Atmost, 4)
	     , A.size = (A.Atleast, 6)
	     , A.distance  = (A.Atleast, 2)
	     , A.optimize = "Größe"
	     }
    , A.make $ A.Config
             { A.length = (A.Atmost, 9)
	     , A.size   = (A.Atleast, 10)
	     , A.distance  = (A.Atleast, 5)
	     , A.optimize = "Länge"
	     }
    , A.make $ A.Config
             { A.length = (A.Atmost, 8)
	     , A.size   = (A.Atleast, 10)
	     , A.distance  = (A.Atleast, 3)
	     , A.optimize = "Weite"
	     }
    , H.make $ H.Config 
	     { H.alphabet = mkSet [ 'a' .. 'i' ]
	     , H.range    = ( 1, 50 )
	     }
    , C.enc  $ C.Config
	     { C.coder    = Code.Move_To_Front.coder
	     , C.alphabet = mkSet [ 'a' .. 'e' ]
	     , C.length_range = ( 10, 13 )
	     }
    , C.dec  $ C.Config
	     { C.coder    = Code.Move_To_Front.coder
	     , C.alphabet = mkSet [ 'b' .. 'e' ]
	     , C.length_range = ( 11, 14 )
	     }
    , C.enc  $ C.Config
	     { C.coder    = Code.Burrows_Wheeler.coder
	     , C.alphabet = mkSet [ 'a' .. 'c' ]
	     , C.length_range = ( 12, 13 )
	     }
    , C.dec  $ C.Config
	     { C.coder    = Code.Burrows_Wheeler.coder
	     , C.alphabet = mkSet [ 'a' .. 'f' ]
	     , C.length_range = ( 11, 14 )
	     }
    ]

 
     