module HTWK.SS04.Informatik.Crypt where

--  $Id$

import qualified Faktor.Quiz as F

import Inter.Types

generates :: [ IO Variant ]
generates = 
    [ F.make $ F.Param
             { F.von = 100
	     , F.bis = 1000
	     , F.anzahl = 3
	     }
    ]

 
     