module HTWK.SS04.Informatik.Crypt where

--  $Id$

import qualified Faktor.Quiz as F
import Faktor.RSA

import qualified RSA.Quiz as R

import Inter.Types

generates :: [ IO Variant ]
generates = 
    [ F.make $ F.Param
             { F.von = 100
	     , F.bis = 1000
	     , F.anzahl = 3
	     }
    , F.fixed "Faktor" "Small"  999835010541675870768950170379
    , F.fixed "Faktor" "Medium" 243037354477218808600931
    , F.fixed "Faktor" "Large"  50035787538396052798619
    , F.fixed "Faktor" "RSA640" rsa640

    , R.make $ R.Param
	     { R.von =  50
	     , R.bis = 150
	     }
    , R.fixed "Break" "Demo" $ R.Config
	     { R.public_key = ( 7, 55 )
	     , R.message = 9
	     }
    ]

 
     
